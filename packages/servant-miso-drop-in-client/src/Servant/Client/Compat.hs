{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Client.Compat
  ( BaseUrl
  , Client
  , ClientEnv
  , ClientError(..)
  , ClientRequest
  , Manager
  , Scheme(..)
  , clientErrorStatus
  , clientWithEnv
  , consoleError
  , consoleLog
  , mkBaseUrl
  , mkClientEnv
  , newManager
  , runClient
  ) where


import Data.Proxy (Proxy)
import Data.Text (Text)
import Control.Exception (SomeException)

#ifdef VANILLA
import qualified Data.Text as Text
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text.IO as TextIO
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Types.Status as HttpStatus
import System.IO (stderr)
import qualified Servant.Client as NativeServantClient
#else
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as Exception
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

newtype BaseUrl 
#ifdef VANILLA
  = BaseUrl NativeServantClient.BaseUrl
#else
  = BaseUrl MisoString
#endif

newtype ClientEnv
#ifdef VANILLA
  = ClientEnv NativeServantClient.ClientEnv
#else
  = ClientEnv BaseUrl
#endif

data ClientError
  = HttpError Int Text
  | RequestException SomeException
  | InvalidResponse (Maybe Int) Text
  deriving stock (Show)

clientErrorStatus :: ClientError -> Maybe Int
clientErrorStatus (HttpError code _) = Just code
clientErrorStatus (InvalidResponse code _) = code
clientErrorStatus (RequestException _) = Nothing

#ifdef VANILLA
fromNativeClientError :: NativeServantClient.ClientError -> ClientError
fromNativeClientError err = case err of
  NativeServantClient.FailureResponse _ response -> HttpError (responseStatus response) (Text.pack (show err))
  NativeServantClient.DecodeFailure message response -> InvalidResponse (Just (responseStatus response)) message
  NativeServantClient.UnsupportedContentType _ response -> InvalidResponse (Just (responseStatus response)) (Text.pack (show err))
  NativeServantClient.InvalidContentTypeHeader response -> InvalidResponse (Just (responseStatus response)) (Text.pack (show err))
  NativeServantClient.ConnectionError exception -> RequestException exception
  where
    responseStatus :: NativeServantClient.ResponseF a -> Int
    responseStatus = HttpStatus.statusCode . NativeServantClient.responseStatusCode
#else
fromBrowserClientError :: Response MisoString -> ClientError
fromBrowserClientError response = case status response of
  Just code | code >= 100 && code < 600 && (code < 200 || code >= 300) -> HttpError code message
  code -> InvalidResponse code message
  where
    message = maybe (fromMisoString (body response)) fromMisoString (errorMessage response)
#endif

#ifdef VANILLA
-- We must make this a newtype so we can pass it as a parameter to 
-- the Client type from Servant
newtype ClientRequest a = ClientRequest (IO (Either ClientError a))
#else
type ClientRequest a =
  (Response a -> IO ())
  -> (Response MisoString -> IO ())
  -> IO ()
#endif

type Client api
#ifdef VANILLA
  = NativeServantClient.Client ClientRequest api
#else
  = MisoClient.ClientType api
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

#ifdef VANILLA
runClient :: ClientRequest a -> IO (Either ClientError a)
runClient (ClientRequest request) = request
#else
runClient :: forall a. ClientRequest a -> IO (Either ClientError a)
runClient request = do
  result <- newEmptyMVar :: IO (MVar (Either SomeException (Either ClientError a)))
  let complete action = do
        outcome <- Exception.try action
        putMVar result outcome
  request
    (\response -> complete (Right <$> Exception.evaluate (body response)))
    (\response -> complete (Left <$> Exception.evaluate (fromBrowserClientError response)))
  outcome <- takeMVar result
  either Exception.throwIO pure outcome
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
    nativeRunClientM request = ClientRequest $
      Bifunctor.first fromNativeClientError <$> NativeServantClient.runClientM request env
#else
clientWithEnv (ClientEnv (BaseUrl url)) = MisoClient.toClient url
#endif

