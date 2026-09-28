{-# LANGUAGE CPP #-}

module Servant.Client.Compat
  ( BaseUrl
  , Client
  , ClientAsync
  , ClientEnv
  , ClientError
  , ClientRequest
  , Manager
  , RetryPolicy(..)
  , Scheme(..)
  , awaitClient
  , clientErrorStatus
  , clientWithEnv
  , consoleError
  , consoleLog
  , mkBaseUrl
  , mkClientEnv
  , newManager
  , noRetry
  , runClient
  , runClientAsync
  ) where


import Data.Proxy (Proxy)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (SomeException, catch)

#ifdef VANILLA
import Control.Concurrent.Async (Async, async, wait)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text.IO as TextIO
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Types.Status as HttpStatus
import System.IO (stderr)
import qualified Servant.Client as NativeServantClient
#else
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Monad (void)
import qualified Miso.FFI as MisoFFI
import Miso.FFI (Response(body, status))
import Miso.String (MisoString, ms)
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

newtype ClientAsync a
#ifdef VANILLA
  = ClientAsync (Async a)
#else
  = ClientAsync (MVar a)
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
  NativeServantClient.FailureResponse _ response -> responseStatus response
  NativeServantClient.DecodeFailure _ response -> responseStatus response
  NativeServantClient.UnsupportedContentType _ response -> responseStatus response
  NativeServantClient.InvalidContentTypeHeader response -> responseStatus response
  NativeServantClient.ConnectionError _ -> Nothing
  where
    responseStatus :: NativeServantClient.ResponseF a -> Maybe Int
    responseStatus = Just . HttpStatus.statusCode . NativeServantClient.responseStatusCode
#else
clientErrorStatus (ClientError response) = status response
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
  BaseUrl . ms $ schemePrefix scheme <> "//" <> host <> ":" <> show port <> path
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

data RetryPolicy m a b where
  RetryPolicy
    :: state
    -> (state -> ClientError -> IO (Either state (m b)))
    -> (a -> m b)
    -> (SomeException -> m b)
    -> RetryPolicy m a b

noRetry :: (ClientError -> m b) -> (a -> m b) -> (SomeException -> m b) -> RetryPolicy m a b
noRetry onError onSuccess = RetryPolicy () (\_ err -> pure (Right (onError err))) onSuccess

#ifdef VANILLA
applyRetryPolicy
  :: RetryPolicy m a b
  -> IO (Either ClientError a)
  -> IO (m b)
applyRetryPolicy (RetryPolicy initialAcc handleError handleSuccess handleException) action =
  go initialAcc `catch` (pure . handleException)
  where
  go acc = do
    result <- action
    case result of
      Left err -> do
        decision <- handleError acc err
        case decision of
          Left newAcc -> go newAcc
          Right terminal -> pure terminal
      Right value -> pure (handleSuccess value)
#endif

runClientAsync :: forall m a b. RetryPolicy m a b -> ClientRequest a -> IO (ClientAsync (m b))
#ifdef VANILLA
runClientAsync policy (ClientRequest request) =
  ClientAsync <$> async (applyRetryPolicy policy request)
#else

runClientAsync (RetryPolicy initialAcc handleError handleSuccess handleException) request = do
  result <- newEmptyMVar
  let
    complete :: m b -> IO ()
    complete = void . tryPutMVar result
    onException :: SomeException -> IO ()
    onException = complete . handleException
    startAttempt acc = request onSuccess onFailure `catch` onException
      where
        onSuccess response = complete (handleSuccess (body response))
        onFailure response =
          (do
            decision <- handleError acc (ClientError response)
            case decision of
              Left newAcc -> startAttempt newAcc
              Right terminal -> complete terminal)
          `catch` onException
  startAttempt initialAcc
  pure (ClientAsync result)
#endif

runClient :: MonadIO m => RetryPolicy m a b -> ClientRequest a -> m b
#ifdef VANILLA
runClient policy (ClientRequest request) = liftIO (applyRetryPolicy policy request) >>= id
#else
runClient policy request = awaitClient =<< liftIO (runClientAsync policy request)
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
      Bifunctor.first ClientError <$> NativeServantClient.runClientM request env
#else
clientWithEnv (ClientEnv (BaseUrl url)) = MisoClient.toClient url
#endif

awaitClient :: MonadIO m => ClientAsync (m b) -> m b
#ifdef VANILLA
awaitClient (ClientAsync asyncRequest) = liftIO (wait asyncRequest) >>= id
#else
awaitClient (ClientAsync result) = liftIO (readMVar result) >>= id
#endif
