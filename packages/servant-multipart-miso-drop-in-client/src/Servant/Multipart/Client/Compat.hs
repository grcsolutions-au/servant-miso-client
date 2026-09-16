{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Multipart.Client.Compat
  ( WithBoundary
  , withBoundary
  , suggestedBoundary
  )
where

import Data.Proxy (Proxy(Proxy))
import Servant.API ((:>))
import Servant.Multipart.API.Compat (MultipartCompat)
import Servant.Multipart.API (MultipartForm')
import qualified Data.ByteString.Lazy as LBS

#ifdef VANILLA
import Control.Monad.IO.Class (MonadIO, liftIO)
import Servant.Client.Core (HasClient(..))
import Servant.Multipart.API (ToMultipart)
import Servant.Multipart.Client
  ( MultipartClient(..)
  , genBoundary
  , genericClientWithRoute
  , genericHoistClientMonad
  )
#else
import Control.Monad (forM_, void)
import Miso (JSVal)
import Miso.DSL (jsg, new, toJSVal)
import Miso.FFI (callFunction)
import Miso.String (MisoString, ms)
import Servant.Miso.Client
  ( HasClient (ClientType, toClientInternal)
  , Request (_reqBody)
  )
import Servant.Multipart.API
  ( MultipartData
  , ToMultipart(toMultipart)
  , fdFileName
  , fdInputName
  , fdPayload
  , files
  , iName
  , iValue
  , inputs
  )
#endif

#ifdef VANILLA
data WithBoundary a = WithBoundary {-# UNPACK #-} !LBS.ByteString a
#else
newtype WithBoundary a = WithBoundary a
#endif 

{-|
The native code 'servant-client' is much more general and can do things 
like run pure monads (as in, like a mock client that doesn't actually 
perform HTTP requests). So there's no guarentee that the monad
'servant-client' is running in can even access IO.

But multipart requests require a boundary line to separate sections.

To ensure this doesn't actually clash with actual file content generally
this is randomly generated for each request and around 50-70 characters long,
and this is long enough that a collision with the actual file content is 
extremely unlikely (like hash collisions level unlikely). 

But there's no way in 'servant-client's interface to call IO functions.

So we instead provide the function 'withBoundary' to add a random boundary to the
contents of the request, which can then be passed to the servant-client machinery. 

When one is running in WASM/JavaScript mode though, we use direct form creation
to construct the multipart form directly in the browser environment, and it
handles the boundary itself, so in that mode 'withBoundary' is essentially a no-op.

But we still provide the 'withBoundary' function for consistency,
so the same code can be used in both vanilla and WASM/JavaScript environments.
-}
#ifdef VANILLA
withBoundary :: MonadIO m => a -> m (WithBoundary a)
withBoundary x = (\y -> WithBoundary y x) <$> liftIO genBoundary
#else
withBoundary :: Monad m => a -> m (WithBoundary a)
withBoundary = pure . WithBoundary
#endif

{-|
There may be cases where one wants to run 'servant-client' in pure code so you
can explicitly provide a boundary using this function.
-}
suggestedBoundary :: LBS.ByteString -> a -> WithBoundary a
#ifdef VANILLA
suggestedBoundary = WithBoundary
#else
suggestedBoundary = const WithBoundary
#endif

#ifdef VANILLA
instance MultipartClient tag => MultipartClient (MultipartCompat tag) where
  loadFile _ = loadFile (Proxy @tag)

instance
  ( MultipartClient tag
  , ToMultipart (MultipartCompat tag) a
  , HasClient m api
  ) => HasClient m (MultipartForm' mods (MultipartCompat tag) a :> api) where
  type Client m (MultipartForm' mods (MultipartCompat tag) a :> api) =
    WithBoundary a -> Client m api
  clientWithRoute pm p req (WithBoundary boundary body) =
    genericClientWithRoute pm p req (boundary, body)
  hoistClientMonad pm p f client (WithBoundary boundary body) =
    genericHoistClientMonad pm p f
      (\(boundary', body') -> client (WithBoundary boundary' body'))
      (boundary, body)

#else

multipartBody :: MultipartData (MultipartCompat tag) -> IO JSVal
multipartBody multipartData = do
  formData <- new (jsg "FormData") ([] :: [MisoString])
  forM_ (inputs multipartData) $ \inputPart ->
    void $ callFunction formData "append" (ms (iName inputPart), ms (iValue inputPart))
  forM_ (files multipartData) $ \filePart -> do
    payloadBytes <- toJSVal (fdPayload filePart)
    file <- new (jsg "File") ([payloadBytes], ms (fdFileName filePart))
    void $ callFunction formData "append" (ms (fdInputName filePart), file)
  pure formData

instance
  ( HasClient api
  , ToMultipart (MultipartCompat tag) a
  ) => HasClient (MultipartForm' mods (MultipartCompat tag) a :> api) where
  type ClientType (MultipartForm' mods (MultipartCompat tag) a :> api) = 
    WithBoundary a -> ClientType api
  toClientInternal _ req (WithBoundary body) =
    toClientInternal
      (Proxy @api)
      (req { _reqBody = Just (multipartBody (toMultipart @(MultipartCompat tag) body)) })
#endif
