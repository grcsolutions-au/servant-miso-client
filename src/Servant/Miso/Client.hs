-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE NamedFieldPuns        #-}
-----------------------------------------------------------------------------
module Servant.Miso.Client
  ( HasClient (..)
  , MimeRender (..)
  , MimeUnrender (..)
  , toClient
  , Request (..)
  ) where
-----------------------------------------------------------------------------
import           Miso.JSON
import qualified Data.Map.Strict as M
import           GHC.TypeLits
import           Data.Proxy
import           Servant.API hiding (MimeRender(..), MimeUnrender(..))
import           Servant.API.MultiVerb
  ( AsHeaders (..)
  , AsUnion (..)
  , MultiVerb
  , Respond
  , RespondAs
  , ResponseType
  , ResponseTypes
  , ServantHeaders (..)
  , UnrenderResult (..)
  , WithHeaders
  )
import           Servant.API.UVerb (HasStatuses)
import           Data.Kind
import           Data.Map (Map)
import qualified Data.List.NonEmpty as NE
import           Data.SOP.BasicFunctors (I (I))
import           Data.SOP.NS (NS (S, Z))
import qualified Data.CaseInsensitive as CI
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HTTP
import           Network.HTTP.Types.Status (statusCode)
import           Servant.API.Status (statusFromNat)
-----------------------------------------------------------------------------
import           Miso (JSVal, ToJSVal(..), fromJSValUnchecked, CONTENT_TYPE(..))
import           Miso.FFI
  ( fetch
  , fetchResponsePlan
  , ResponsePlan(..)
  , ResponseVariant(..)
  , ResponseRepresentation(..)
  , Blob
  , ArrayBuffer
  , File
  , URLSearchParams
  , FormData
  , Response(..)
  )
import           Miso.String
import qualified Miso.String as MS
-----------------------------------------------------------------------------
class HasClient (api :: k) where
  type ClientType api :: Type
  toClientInternal
    :: Proxy api
    -> Request
    -> ClientType api
-----------------------------------------------------------------------------
toClient
  :: HasClient api
  => MisoString
  -> Proxy api
  -> ClientType api
toClient baseUrl api =
  toClientInternal api emptyRequestState { _baseUrl = baseUrl }
-----------------------------------------------------------------------------
emptyRequestState :: Request
emptyRequestState = Request mempty mempty Nothing mempty mempty mempty (ms "")
-----------------------------------------------------------------------------
data Request
  = Request
  { _headers :: Map MisoString MisoString
  , _queryParams :: Map MisoString MisoString
  , _reqBody :: Maybe (IO JSVal)
  , _flags :: [MisoString]
  , _paths :: [MisoString]
  , _frags :: [MisoString]
  , _baseUrl :: MisoString
  }
-----------------------------------------------------------------------------
class Accept ctyp => MimeRender ctyp a where
  type MimeRenderType a :: Type
  mimeRender :: Proxy ctyp -> a -> MimeRenderType a
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSON a) => MimeRender JSON a where
  type MimeRenderType a = IO JSVal
  mimeRender Proxy x = toJSVal (encode x)
-----------------------------------------------------------------------------
instance MimeRender OctetStream Blob where
  type MimeRenderType Blob = IO JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
instance MimeRender OctetStream ArrayBuffer where
  type MimeRenderType ArrayBuffer = IO JSVal
  mimeRender Proxy = toJSVal
----------------------------------------------------------------------------
instance MimeRender OctetStream File where
  type MimeRenderType File = IO JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
instance MimeRender FormUrlEncoded URLSearchParams where
  type MimeRenderType URLSearchParams = IO JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
instance MimeRender FormUrlEncoded FormData where
  type MimeRenderType FormData = IO JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
instance MimeRender PlainText MisoString where
  type MimeRenderType MisoString = IO JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
class Accept ctyp => MimeUnrender ctyp a where
  type MimeUnrenderType a :: Type
  mimeUnrenderType :: Proxy ctyp -> Proxy a -> CONTENT_TYPE
  mimeUnrender :: Proxy ctyp -> MimeUnrenderType a -> IO (Either MisoString a)
-----------------------------------------------------------------------------
instance MimeUnrender OctetStream File where
  type MimeUnrenderType File = JSVal
  mimeUnrenderType Proxy Proxy = BLOB
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked
-----------------------------------------------------------------------------
instance MimeUnrender OctetStream Blob where
  type MimeUnrenderType Blob = JSVal
  mimeUnrenderType Proxy Proxy = BLOB
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked
-----------------------------------------------------------------------------
instance MimeUnrender OctetStream ArrayBuffer where
  type MimeUnrenderType ArrayBuffer = JSVal
  mimeUnrenderType Proxy Proxy = ARRAY_BUFFER
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked
-----------------------------------------------------------------------------
instance MimeUnrender PlainText MisoString where
  type MimeUnrenderType MisoString = JSVal
  mimeUnrenderType Proxy Proxy = TEXT
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked
-----------------------------------------------------------------------------
instance FromJSON json => MimeUnrender JSON json where
  type MimeUnrenderType json = JSVal
  mimeUnrenderType Proxy Proxy = JSON
  mimeUnrender Proxy jval = do
    value :: Value <- fromJSValUnchecked jval
    pure $ case fromJSON @json value of
      Success result -> Right result
      Error message -> Left (ms message)
-----------------------------------------------------------------------------
-- Servant's unit response is represented by an intentional empty fetch body.
instance Accept ct => MimeUnrender ct NoContent where
  type MimeUnrenderType NoContent = JSVal
  mimeUnrenderType Proxy Proxy = NONE
  mimeUnrender Proxy _ = pure (Right NoContent)

-- Preserve the raw response body when decoding fails so the error callback
-- retains the status, headers, and original payload.
handleDecodedResponse
  :: Response JSVal
  -> IO (Either MisoString a)
  -> (a -> b)
  -> (Response b -> IO ())
  -> (Response MisoString -> IO ())
  -> IO ()
handleDecodedResponse resp decoder toResult successful errorful =
  decoder >>= \case
    Left errorMessage_ -> do
      body_ <- fromJSValUnchecked (body resp)
      errorful resp { errorMessage = Just errorMessage_, body = body_ }
    Right result -> successful resp { body = toResult result }

-----------------------------------------------------------------------------
class MisoAccept (cts :: [Type]) where
  misoAcceptHeader :: Proxy cts -> MisoString

instance MisoAccept '[] where
  misoAcceptHeader Proxy = ms ""

instance (Accept ct, MisoAccept cts) => MisoAccept (ct ': cts) where
  -- Native servant expands each content type with 'contentTypes', not just
  -- its legacy singular 'contentType' method. A custom Accept instance may
  -- therefore contribute several media types for one type-level element.
  misoAcceptHeader Proxy =
    let current = MS.intercalate (ms ", ") $
          fmap (ms . show) (NE.toList (contentTypes (Proxy @ct)))
        rest = misoAcceptHeader (Proxy @cts)
    in if MS.null rest then current else current <> ms ", " <> rest
  -- UVerb alternatives become a response plan so Miso can select the fixed
  -- response representation by status before Haskell decodes the union member.
instance (KnownSymbol path, HasClient api) => HasClient (path :> api) where
  type ClientType (path :> api) = ClientType api
  toClientInternal Proxy req@Request{..} =
    toClientInternal (Proxy @api) req { _paths = _paths ++ [path] }
      where
        path = ms $ symbolVal (Proxy @path)
-----------------------------------------------------------------------------
instance (HasClient left, HasClient right) => HasClient (left :<|> right) where
  type ClientType (left :<|> right) = ClientType left :<|> ClientType right
  toClientInternal Proxy req =
    toClientInternal (Proxy @left) req :<|>
      toClientInternal (Proxy @right) req
-----------------------------------------------------------------------------
instance (ToMisoString a, HasClient api) => HasClient (Capture name a :> api) where
  type ClientType (Capture name a :> api) = a -> ClientType api
  toClientInternal Proxy req@Request{..} x =
    toClientInternal (Proxy @api) req { _paths = _paths ++ [toMisoString x] }
-----------------------------------------------------------------------------
instance (MimeRender t a, HasClient api) => HasClient (ReqBody (t ': ts) a :> api) where
  type ClientType (ReqBody (t ': ts) a :> api) = a -> ClientType api
  toClientInternal Proxy req body = toClientInternal (Proxy @api) req {
    _reqBody = Just (mimeRender (Proxy @t) body)
  , _headers = _headers req <> M.singleton (ms "Content-Type") (ms (show (contentType (Proxy @t))))
  }
-----------------------------------------------------------------------------
instance (KnownSymbol name, ToMisoString a, HasClient api) => HasClient (Header name a :> api) where
  type ClientType (Header name a :> api) = a -> ClientType api
  toClientInternal Proxy request@Request{..} x =
    toClientInternal (Proxy @api) request
      { _headers = M.insert name (toMisoString x) _headers
      } where
          name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance (KnownSymbol name, HasClient api) => HasClient (QueryFlag name :> api) where
  type ClientType (QueryFlag name :> api) = Bool -> ClientType api
  toClientInternal _ req@Request{..} hasFlag =
    toClientInternal (Proxy @api) req {
      _flags = _flags ++ [ name | hasFlag ]
    } where
        name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance HasClient EmptyAPI where
  type ClientType EmptyAPI = IO ()
  toClientInternal _ _ = pure ()
-----------------------------------------------------------------------------
instance (ToMisoString a, HasClient api, KnownSymbol name) => HasClient (QueryParam name a :> api) where
  type ClientType (QueryParam name a :> api) = Maybe a -> ClientType api
  toClientInternal Proxy request = \case
    Nothing ->
      toClientInternal (Proxy @api) request
    Just param ->
      toClientInternal (Proxy @api) request {
        _queryParams = M.insert name (ms param) (_queryParams request)
      } where
          name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance (ToMisoString a, HasClient api) => HasClient (Fragment a :> api) where
  type ClientType (Fragment a :> api) = a -> ClientType api
  toClientInternal Proxy req@Request {..} frag =
    toClientInternal (Proxy @api) req {
      _frags = _frags ++ [ms frag]
    }
-----------------------------------------------------------------------------
-- | This response can be 'json', 'text', 'arrayBuffer', 'blob', or 'none'.
--
-- The fixed-response path used to call 'fetch' directly. That made the
-- generated client treat every successful 2xx response as a success because
-- the browser-side helper only had 'Response.ok' available for its decision.
-- Native servant-client-core instead passes the status from the API type to
-- 'runRequestAcceptStatus', so a 'Verb method 201 ...' must reject a 200 and
-- a 'Verb method 204 ...' must reject a 200 or 205. This instance now sends a
-- one-variant response plan for the same exact-status check.
--
-- This is an intentional change for ordinary 'Verb' endpoints: a response
-- with a different 2xx status now reaches the error callback. It is not a
-- change to the dedicated 'NoContentVerb' instance below, whose historical
-- contract is to accept any successful status and to return 'Response ()'.
-- Keeping that separate instance preserves the existing public behavior of
-- 'PostNoContent' and the other method-specific no-content aliases.
instance
  ( KnownNat code
  , MisoMimeUnrender '[t] response
  , MisoAccept '[t]
  , ReflectMethod method
  ) => HasClient (Verb method code (t ': ts) response) where
  type ClientType (Verb method code (t ': ts) response)
     = (Response response -> IO ())
    -> (Response MisoString -> IO ())
    -> IO ()
  toClientInternal Proxy req@Request {..} successful errorful = do
    body_ <- sequenceA _reqBody
    fetchResponsePlan (makeFullPath req) method body_
      (M.toList (_headers <> acceptHeader)) successed errorful
      (ResponsePlan [responseVariant])
        where
          method = ms $ reflectMethod (Proxy @method)
          acceptHeader = M.singleton (ms "Accept")
            (misoAcceptHeader (Proxy @'[t]))
          responseVariant = ResponseVariant
            (statusCode (statusFromNat (Proxy @code)))
            (misoMimeRepresentation (Proxy @'[t]) (Proxy @response))
          successed resp@Response { body } =
            handleDecodedResponse resp
              (misoMimeUnrender (Proxy @'[t]) body)
              id successful errorful
-----------------------------------------------------------------------------
class UVerbResponse (cts :: [Type]) (as :: [Type]) where
  uVerbResponseVariants :: Proxy cts -> Proxy as -> [ResponseVariant]
  uVerbDecode
    :: Proxy cts
    -> Proxy as
    -> Maybe Int
    -> JSVal
    -> IO (Either MisoString (Union as))

instance UVerbResponse cts '[] where
  uVerbResponseVariants Proxy Proxy = []
  uVerbDecode Proxy Proxy _ _ =
    pure (Left (ms "No matching UVerb response status"))

instance {-# OVERLAPPABLE #-}
  ( HasStatus a
  , MisoMimeUnrender cts a
  , UVerbResponse cts as
  ) => UVerbResponse cts (a ': as) where
  uVerbResponseVariants Proxy Proxy =
    ResponseVariant
      (statusCode (statusOf (Proxy @a)))
      (misoMimeRepresentation (Proxy @cts) (Proxy @a)) :
      uVerbResponseVariants (Proxy @cts) (Proxy @as)
  uVerbDecode Proxy Proxy responseStatus body
    | responseStatus == Just (statusCode (statusOf (Proxy @a))) = do
        -- The status branch is known to be the head of the union. Constructing
        -- that branch directly avoids an ambiguous UElem search while keeping
        -- the same ordered union value as servant-client-core.
        fmap (fmap (Z . I))
          (misoMimeUnrender (Proxy @cts) body)
    | otherwise = fmap (fmap S)
        (uVerbDecode (Proxy @cts) (Proxy @as) responseStatus body)
-----------------------------------------------------------------------------
instance {-# OVERLAPPING #-}
  ( HasStatus (WithStatus status a)
  , MisoMimeUnrender cts a
  , UVerbResponse cts as
  ) => UVerbResponse cts (WithStatus status a ': as) where
  uVerbResponseVariants Proxy Proxy =
    ResponseVariant
      (statusCode (statusOf (Proxy @(WithStatus status a))))
      (misoMimeRepresentation (Proxy @cts) (Proxy @a)) :
      uVerbResponseVariants (Proxy @cts) (Proxy @as)
  uVerbDecode Proxy Proxy responseStatus body
    | responseStatus == Just
      (statusCode (statusOf (Proxy @(WithStatus status a)))) = do
      fmap (fmap (Z . I . WithStatus))
        (misoMimeUnrender (Proxy @cts) body)
    | otherwise = fmap (fmap S)
      (uVerbDecode (Proxy @cts) (Proxy @as) responseStatus body)
-----------------------------------------------------------------------------
instance
  ( MisoAccept (ct ': cts)
  , ReflectMethod method
  , HasStatuses as
  , Unique (Statuses as)
  , UVerbResponse (ct ': cts) as
  ) => HasClient (UVerb method (ct ': cts) as) where
  type ClientType (UVerb method (ct ': cts) as)
     = (Response (Union as) -> IO ())
    -> (Response MisoString -> IO ())
    -> IO ()
  toClientInternal Proxy req@Request {..} successful errorful = do
    body_ <- sequenceA _reqBody
    fetchResponsePlan (makeFullPath req) method body_
      (M.toList (_headers <> acceptHeader)) successed errorful
      (ResponsePlan
        (uVerbResponseVariants (Proxy @(ct ': cts)) (Proxy @as))
        )
    where
      method = ms $ reflectMethod (Proxy @method)
      acceptHeader = M.singleton (ms "Accept")
        (misoAcceptHeader (Proxy @(ct ': cts)))
      successed :: Response JSVal -> IO ()
      successed resp@Response
        { status = responseStatus
        , body
        } = do
        handleDecodedResponse resp
          (uVerbDecode (Proxy @(ct ': cts)) (Proxy @as)
            responseStatus body)
          id successful errorful
-----------------------------------------------------------------------------
class MisoMimeUnrender (cs :: [Type]) a where
  -- A planned response has one representation. If the type-level content
  -- list contains more than one type, the first one supplies both the
  -- browser reader and the Haskell parser; this is intentionally not
  -- compile-time enforced.
  misoMimeRepresentation :: Proxy cs -> Proxy a -> ResponseRepresentation
  misoMimeUnrender :: Proxy cs -> JSVal -> IO (Either MisoString a)

instance MimeUnrender ct a => MisoMimeUnrender (ct ': cts) a where
  misoMimeRepresentation Proxy Proxy =
    mimeRepresentation (Proxy @ct) (Proxy @a)
  misoMimeUnrender Proxy = mimeUnrender (Proxy @ct)

-- The first declared media type supplies the planned response representation.
-- Additional media types remain visible in the request Accept header, but are
-- not negotiated at runtime. A server returning one of them may therefore be
-- decoded with the first type's reader and parser.
mimeRepresentation
  :: forall ctyp a. MimeUnrender ctyp a
  => Proxy ctyp
  -> Proxy a
  -> ResponseRepresentation
mimeRepresentation Proxy Proxy =
  let bodyType = mimeUnrenderType (Proxy @ctyp) (Proxy @a)
  in if bodyType == NONE
      then ResponseRepresentation Nothing NONE
      else ResponseRepresentation
        (Just $ ms (show $ NE.head (contentTypes (Proxy @ctyp)))) bodyType
-----------------------------------------------------------------------------
class MultiVerbResponse (cs :: [Type]) response where
  multiVerbResponseStatus :: Proxy cs -> Proxy response -> Int
  multiVerbResponseVariant :: Proxy cs -> Proxy response -> ResponseVariant
  multiVerbResponseDecode
    :: Proxy cs
    -> Proxy response
    -> JSVal
    -> Map MisoString MisoString
    -> IO (UnrenderResult (ResponseType response))

instance
  ( KnownNat status
  , MisoMimeUnrender cs a
  ) => MultiVerbResponse cs (Respond status description a) where
  multiVerbResponseStatus Proxy Proxy = fromIntegral (natVal (Proxy @status))
  multiVerbResponseVariant Proxy Proxy = ResponseVariant
    (multiVerbResponseStatus (Proxy @cs) (Proxy @(Respond status description a)))
    (misoMimeRepresentation (Proxy @cs) (Proxy @a))
  multiVerbResponseDecode Proxy Proxy body _headers =
    fmap (either (UnrenderError . MS.fromMisoString) UnrenderSuccess)
      (misoMimeUnrender (Proxy @cs) body)

instance {-# OVERLAPPABLE #-}
  ( KnownNat status
  , MimeUnrender responseContentType a
  ) => MultiVerbResponse cs
       (RespondAs responseContentType status description a) where
  multiVerbResponseStatus Proxy Proxy = fromIntegral (natVal (Proxy @status))
  multiVerbResponseVariant Proxy Proxy = ResponseVariant
    (multiVerbResponseStatus
      (Proxy @cs)
      (Proxy @(RespondAs responseContentType status description a)))
    (mimeRepresentation (Proxy @responseContentType) (Proxy @a))
  multiVerbResponseDecode Proxy Proxy body _ =
    fmap (either (UnrenderError . MS.fromMisoString) UnrenderSuccess)
      (mimeUnrender (Proxy @responseContentType) body)

instance {-# OVERLAPPING #-} KnownNat status => MultiVerbResponse cs
    (RespondAs '() status description ()) where
  multiVerbResponseStatus Proxy Proxy = fromIntegral (natVal (Proxy @status))
  multiVerbResponseVariant Proxy Proxy = ResponseVariant
    (multiVerbResponseStatus
      (Proxy @cs)
      (Proxy @(RespondAs '() status description ())))
    (ResponseRepresentation Nothing NONE)
  -- NONE marks this alternative as intentionally bodyless.
  multiVerbResponseDecode Proxy Proxy _ _ = pure (UnrenderSuccess ())

instance
  ( AsHeaders headerTypes (ResponseType response) returnType
  , ServantHeaders headers headerTypes
  , MultiVerbResponse cs response
  ) => MultiVerbResponse cs (WithHeaders headers returnType response) where
  multiVerbResponseStatus Proxy Proxy =
    multiVerbResponseStatus (Proxy @cs) (Proxy @response)
  multiVerbResponseVariant Proxy Proxy =
    multiVerbResponseVariant (Proxy @cs) (Proxy @response)
  multiVerbResponseDecode Proxy Proxy body headers = do
    multiVerbResponseDecode (Proxy @cs) (Proxy @response)
      body headers >>= \case
      StatusMismatch -> pure StatusMismatch
      UnrenderError errorMessage_ -> pure (UnrenderError errorMessage_)
      UnrenderSuccess responseBody ->
        case extractHeaders @headers (misoHeaders headers) of
          Nothing -> pure (UnrenderError "Failed to parse response headers")
          Just responseHeaders -> pure $ UnrenderSuccess
            (fromHeaders @headerTypes @(ResponseType response) @returnType
              (responseHeaders, responseBody))
-----------------------------------------------------------------------------
class MultiVerbResponses (cs :: [Type]) (responses :: [Type]) where
  multiVerbResponseVariants :: Proxy cs -> Proxy responses -> [ResponseVariant]
  multiVerbDecode
    :: Proxy cs
    -> Proxy responses
    -> Maybe Int
    -> JSVal
    -> Map MisoString MisoString
    -> IO (UnrenderResult (Union (ResponseTypes responses)))

instance MultiVerbResponses cs '[] where
  multiVerbResponseVariants Proxy Proxy = []
  multiVerbDecode Proxy Proxy _ _ _
    =
    pure StatusMismatch

instance
  ( MultiVerbResponse cs response
  , MultiVerbResponses cs responses
  ) => MultiVerbResponses cs (response ': responses) where
  -- This is close to servant-client-core's ordered Alternative traversal, but
  -- it assumes that MultiVerb alternatives use distinct HTTP status codes and
  -- one representation per status. With those invariants, the status
  -- identifies both the response branch and its parser, so there is no need
  -- to inspect Content-Type, retain raw bytes, or retry another reader. If
  -- either invariant is violated, Miso selects the first matching status and
  -- may decode with the wrong reader or parser instead of behaving like
  -- native servant-client-core.
  multiVerbResponseVariants Proxy Proxy =
    multiVerbResponseVariant (Proxy @cs) (Proxy @response) :
      multiVerbResponseVariants (Proxy @cs) (Proxy @responses)
  multiVerbDecode Proxy Proxy responseStatus body headers
    | responseStatus == Just
        (multiVerbResponseStatus (Proxy @cs) (Proxy @response)) = do
        decoded <- multiVerbResponseDecode (Proxy @cs) (Proxy @response)
          body headers
        case decoded of
          StatusMismatch -> pure StatusMismatch
          UnrenderError errorMessage_ -> pure (UnrenderError errorMessage_)
          UnrenderSuccess result ->
            pure (UnrenderSuccess (inject (I result)))
    | otherwise = fmap (fmap S)
        (multiVerbDecode (Proxy @cs) (Proxy @responses)
          responseStatus body headers)
-----------------------------------------------------------------------------
instance
  ( MisoAccept (ct ': cts)
  , AsUnion responses result
  , MultiVerbResponses (ct ': cts) responses
  , ReflectMethod method
  ) => HasClient (MultiVerb method (ct ': cts) responses result) where
  type ClientType (MultiVerb method (ct ': cts) responses result)
     = (Response result -> IO ())
    -> (Response MisoString -> IO ())
    -> IO ()
  toClientInternal Proxy req@Request {..} successful errorful = do
    body_ <- sequenceA _reqBody
    fetchResponsePlan (makeFullPath req) method body_
      (M.toList (_headers <> acceptHeader)) successed errorful
      (ResponsePlan
        (multiVerbResponseVariants (Proxy @(ct ': cts)) (Proxy @responses)))
    where
      method = ms $ reflectMethod (Proxy @method)
      acceptHeader = M.singleton (ms "Accept")
        (misoAcceptHeader (Proxy @(ct ': cts)))
      successed resp@Response
        { status = responseStatus
        , body
        , headers
        } = do
        handleDecodedResponse resp
          (fmap multiVerbResultToEither
            (multiVerbDecode (Proxy @(ct ': cts)) (Proxy @responses)
              responseStatus body headers))
          (fromUnion @responses) successful errorful
      multiVerbResultToEither :: UnrenderResult a -> Either MisoString a
      multiVerbResultToEither = \case
        StatusMismatch -> Left (ms "No matching MultiVerb response status")
        UnrenderError errorMessage_ -> Left (ms errorMessage_)
        UnrenderSuccess result -> Right result
-----------------------------------------------------------------------------
instance ReflectMethod method => HasClient (NoContentVerb method) where
  type ClientType (NoContentVerb method)
     = (Response () -> IO ())
    -> (Response MisoString -> IO ())
    -> IO ()
  toClientInternal Proxy req@Request {..} successful errorful = do
    body_ <- sequenceA _reqBody
    fetch (makeFullPath req) method body_ (M.toList (_headers <> acceptHeader))
      successful errorful NONE
        where
          method = ms $ reflectMethod (Proxy @method)
          acceptHeader = M.singleton (ms "Accept") (ms "*/*")
-----------------------------------------------------------------------------
makeFullPath :: Request -> MisoString
makeFullPath Request {..} = path <> queryParams <> queryFlags <> fragments
  where
    basedUrl
      | _baseUrl == ms "/" = _baseUrl
      | MS.null _baseUrl = ms "/" -- dmj: relative url
      | otherwise = _baseUrl <> ms "/"
    path = basedUrl <> MS.intercalate (ms "/") _paths
    queryParams = MS.concat
      [ ms "?" <>
        MS.intercalate (ms "&")
        [ k <> ms "=" <> v
        | (k,v) <- M.toList _queryParams
        ]
      | M.size _queryParams > 0
      ]
    queryFlags = MS.concat [ ms "?" <> x | x <- _flags ]
    fragments = MS.concat [ ms "#" <> x | x <- _frags ]
-----------------------------------------------------------------------------
-- Servant header decoding expects http-types headers rather than Miso's map.
misoHeaders :: Map MisoString MisoString -> Seq.Seq HTTP.Header
misoHeaders = Seq.fromList . fmap toMisoHeader . M.toList
  where
    toMisoHeader (name, value) =
      ( CI.mk (TE.encodeUtf8 (T.pack (MS.fromMisoString name)))
      , TE.encodeUtf8 (T.pack (MS.fromMisoString value))
      )
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient api => HasClient (Host sym :> api) where
  type ClientType (Host sym :> api) = ClientType api
  toClientInternal Proxy req = toClientInternal (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient api => HasClient (HttpVersion :> api) where
  type ClientType (HttpVersion :> api) = ClientType api
  toClientInternal Proxy req = toClientInternal (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient api => HasClient (DeepQuery sym a :> api) where
  type ClientType (DeepQuery sym a :> api) = ClientType api
  toClientInternal Proxy req = toClientInternal (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient api => HasClient (IsSecure :> api) where
  type ClientType (IsSecure :> api) = ClientType api
  toClientInternal Proxy req = toClientInternal (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient api => HasClient (RemoteHost :> api) where
  type ClientType (RemoteHost :> api) = ClientType api
  toClientInternal Proxy req = toClientInternal (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported yet
instance HasClient api => HasClient (CaptureAll sym a :> api) where
  type ClientType (CaptureAll sym a :> api) = ClientType api
  toClientInternal Proxy req = toClientInternal (Proxy @api) req
-----------------------------------------------------------------------------
