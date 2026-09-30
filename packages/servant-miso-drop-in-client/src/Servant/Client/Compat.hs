{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Client.Compat
  ( BaseUrl
  , Client
  , ClientAsync
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
  , runClientSync
  , runClientSyncM
  , waitClient
  , waitClientM
  , withClientAsync
  ) where


import Data.Proxy (Proxy)
import Data.Text (Text)
import Control.Exception (SomeException)
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)

#ifdef VANILLA
import Control.Concurrent.Async (Async, async, wait)
import qualified Data.Text as Text
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text.IO as TextIO
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Types.Status as HttpStatus
import System.IO (stderr)
import qualified Servant.Client as NativeServantClient
#else
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import qualified Miso.FFI as MisoFFI
import Miso.FFI (Response(body, errorMessage, status))
import Miso.String (MisoString, fromMisoString, ms)
import qualified Servant.Miso.Client as MisoClient
#endif

#ifdef VANILLA
newtype ClientAsync a = ClientAsync (Async (Either ClientError a))
#else
newtype ClientAsync a = ClientAsync (MVar (Either ClientError a))
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
newtype ClientRequest a = ClientRequest (IO (Either ClientError a))
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

withClientAsync :: forall a b m. MonadIO m => ClientRequest a -> (ClientAsync a -> m b) -> m b
#ifdef VANILLA
withClientAsync (ClientRequest request) use = do
  asyncResult <- liftIO $ async request
  use (ClientAsync asyncResult)
#else
withClientAsync request use = do
  result <- liftIO $ newEmptyMVar :: m (MVar (Either ClientError a))
  liftIO $ request
    (\response -> putMVar result (Right (body response)))
    (\response -> putMVar result (Left (ClientError response)))
  use (ClientAsync result)
#endif

waitClient :: MonadIO m => ClientAsync a -> m (Either ClientError a)
#ifdef VANILLA
waitClient (ClientAsync worker) = liftIO $ wait worker
#else
waitClient (ClientAsync result) = liftIO $ readMVar result
#endif

waitClientM :: (MonadIO m, MonadError ClientError m) => ClientAsync a -> m a
waitClientM = waitClient >=> liftEither

runClientSync :: MonadIO m => ClientRequest a -> m (Either ClientError a)
#ifdef VANILLA
-- We can avoid all the async stuff in the synchronous case in a native environment
runClientSync (ClientRequest request) = liftIO request
#else
-- But, when we're in a WASM/browser environment, we're basically waiting on an MVar anyway
-- so there's nothing we can do than a withAsync followed by an immediate wait on the MVar.
runClientSync request = withClientAsync request waitClient
#endif 

runClientSyncM :: (MonadIO m, MonadError ClientError m) => ClientRequest a -> m a
runClientSyncM = runClientSync >=> liftEither

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
      Bifunctor.first ClientError <$> NativeServantClient.runClientM request env
#else
clientWithEnv (ClientEnv (BaseUrl url)) = MisoClient.toClient url
#endif
