{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Client.Compat
  ( BaseUrl
  , Client
  , ClientEnv
  , ClientError
  , ClientRequest
  , Manager
  , Scheme(..)
  , clientErrorException
  , clientErrorMessage
  , clientErrorStatus
  , clientWithEnv
  , consoleError
  , consoleLog
  , mkBaseUrl
  , mkClientEnv
  , newManager
  , sendRequest
  ) where


import Data.Proxy (Proxy)
import Data.Text (Text)
import Control.Exception (SomeException)

#ifdef VANILLA
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Types.Status as HttpStatus
import System.IO (stderr)
import qualified Servant.Client as NativeServantClient
#else
import qualified Miso.FFI as MisoFFI
import Miso.FFI (Response(body, errorMessage, status))
import Miso.String (MisoString, fromMisoString, ms)
import qualified Servant.Miso.Client as MisoClient
#endif

data Scheme
  = Http
  | Https

#ifdef VANILLA
newtype Manager = Manager HttpClient.Manager
#else
data Manager = Manager
#endif


#ifdef VANILLA
newtype BaseUrl = BaseUrl NativeServantClient.BaseUrl
#else
newtype BaseUrl = BaseUrl MisoString
#endif


#ifdef VANILLA
newtype ClientEnv = ClientEnv NativeServantClient.ClientEnv
#else
newtype ClientEnv = ClientEnv BaseUrl
#endif

newtype ClientError
#ifdef VANILLA
  = ClientError NativeServantClient.ClientError
#else
  = ClientError (Response MisoString)
#endif

clientErrorStatus :: ClientError -> Maybe Int
#ifdef VANILLA
clientErrorStatus (ClientError err) = case err of
  NativeServantClient.FailureResponse _ response -> Just (responseStatus response)
  NativeServantClient.DecodeFailure _ response -> Just (responseStatus response)
  NativeServantClient.UnsupportedContentType _ response -> Just (responseStatus response)
  NativeServantClient.InvalidContentTypeHeader response -> Just (responseStatus response)
  NativeServantClient.ConnectionError _ -> Nothing
  where
    responseStatus :: NativeServantClient.ResponseF a -> Int
    responseStatus = HttpStatus.statusCode . NativeServantClient.responseStatusCode
#else
clientErrorStatus (ClientError response) = status response
#endif

clientErrorMessage :: ClientError -> Text
#ifdef VANILLA
clientErrorMessage (ClientError err) = case err of
  NativeServantClient.FailureResponse _ _ -> Text.pack (show err)
  NativeServantClient.DecodeFailure message _ -> message
  NativeServantClient.UnsupportedContentType _ _ -> Text.pack (show err)
  NativeServantClient.InvalidContentTypeHeader _ -> Text.pack (show err)
  NativeServantClient.ConnectionError exception -> Text.pack (show exception)
#else
clientErrorMessage (ClientError response) =
  maybe (fromMisoString (body response)) fromMisoString (errorMessage response)
#endif

clientErrorException :: ClientError -> Maybe SomeException
#ifdef VANILLA
clientErrorException (ClientError err) = case err of
  NativeServantClient.ConnectionError exception -> Just exception
  _ -> Nothing
#else
clientErrorException _ = Nothing
#endif

#ifdef VANILLA
-- We must make this a newtype so we can pass it as a parameter to 
-- the Client type from Servant
newtype ClientRequest a = ClientRequest ((a -> IO ()) -> (ClientError -> IO ()) -> IO ())
#else
type ClientRequest a =
  (Response a -> IO ())
  -> (Response MisoString -> IO ())
  -> IO ()
#endif

#ifdef VANILLA
type Client api = NativeServantClient.Client ClientRequest api
#else
type Client api = MisoClient.ClientType api
#endif

consoleLog :: Text -> IO ()
#ifdef VANILLA
consoleLog = TextIO.putStrLn
#else
consoleLog = MisoFFI.consoleLog . ms
#endif

consoleError :: Text -> IO ()
#ifdef VANILLA
consoleError = TextIO.hPutStrLn stderr
#else
consoleError = MisoFFI.consoleError . ms
#endif

newManager :: IO Manager
#ifdef VANILLA
newManager = Manager <$> HttpClient.newManager HttpClient.defaultManagerSettings
#else
newManager = pure Manager
#endif

mkBaseUrl :: Scheme -> String -> Int -> String -> BaseUrl
#ifdef VANILLA
mkBaseUrl scheme host port path =
  BaseUrl $ NativeServantClient.BaseUrl (nativeScheme scheme) host port path
  where
    nativeScheme Http = NativeServantClient.Http
    nativeScheme Https = NativeServantClient.Https
#else
mkBaseUrl scheme host port path =
  BaseUrl $ ms (schemePrefix scheme <> "//" <> host <> ":") <> ms port <> ms path
  where
    schemePrefix Http = "http:"
    schemePrefix Https = "https:"
#endif

mkClientEnv :: Manager -> BaseUrl -> ClientEnv
#ifdef VANILLA
mkClientEnv (Manager manager) (BaseUrl url) =
  ClientEnv (NativeServantClient.mkClientEnv manager url)
#else
mkClientEnv _ = ClientEnv
#endif

clientWithEnv ::
#ifdef VANILLA
  NativeServantClient.HasClient NativeServantClient.ClientM api
#else
  MisoClient.HasClient api
#endif
  => ClientEnv
  -> Proxy api
  -> Client api
#ifdef VANILLA
clientWithEnv (ClientEnv env) api =
  NativeServantClient.hoistClient api nativeRunClientM (NativeServantClient.client api)
  where
    nativeRunClientM :: NativeServantClient.ClientM a -> ClientRequest a
    nativeRunClientM request = ClientRequest $ \onSuccess onError ->
      NativeServantClient.runClientM request env >>= either (onError . ClientError) onSuccess
#else
clientWithEnv (ClientEnv (BaseUrl url)) = MisoClient.toClient url
#endif

sendRequest :: ClientRequest a -> (a -> IO ()) -> (ClientError -> IO ()) -> IO ()
#ifdef VANILLA
sendRequest (ClientRequest run) = run
#else
sendRequest request onSuccess onError = request (onSuccess . body) (onError . ClientError)
#endif
