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
import Control.Exception (SomeException, evaluate)

#ifdef VANILLA
import Control.Concurrent.Async (Async, async, waitCatch)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text.IO as TextIO
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Types.Status as HttpStatus
import System.IO (stderr)
import qualified Servant.Client as NativeServantClient
#else
import Control.Exception (catch)
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

data ClientAsync m result where
#ifdef VANILLA
  ClientAsync :: Async raw -> (raw -> m result) -> (SomeException -> m result) -> ClientAsync m result
#else
  ClientAsync :: MVar (Either SomeException raw) -> (raw -> m result) -> (SomeException -> m result) -> ClientAsync m result
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

data RetryPolicy m a result where
  RetryPolicy
    :: { retryInitialState :: state
       , retryOnError :: state -> ClientError -> IO (Either state raw)
       , retryOnSuccess :: a -> IO raw
       , retryOnException :: SomeException -> m result
       , retryFinish :: raw -> m result
       }
    -> RetryPolicy m a result

noRetry
  :: (ClientError -> IO raw)
  -> (a -> IO raw)
  -> (SomeException -> m result)
  -> (raw -> m result)
  -> RetryPolicy m a result
noRetry onError onSuccess onException finish = RetryPolicy
  { retryInitialState = ()
  , retryOnError = \_ err -> Right <$> onError err
  , retryOnSuccess = onSuccess
  , retryOnException = onException
  , retryFinish = finish
  }

runClientAsync :: MonadIO m => RetryPolicy m a result -> ClientRequest a -> m (ClientAsync m result)
#ifdef VANILLA
runClientAsync RetryPolicy
  { retryInitialState = initialState
  , retryOnError = handleError
  , retryOnSuccess = handleSuccess
  , retryOnException = onException
  , retryFinish = finish
  } (ClientRequest request) = liftIO $ do
  worker <- async (go initialState)
  pure (ClientAsync worker finish onException)
  where
    go state = do
      response <- request
      case response of
        Left err -> do
          decision <- handleError state err
          case decision of
            Left nextState -> go nextState
            Right raw -> evaluate raw
        Right value -> handleSuccess value >>= evaluate
#else

runClientAsync RetryPolicy
  { retryInitialState = initialState
  , retryOnError = handleError
  , retryOnSuccess = handleSuccess
  , retryOnException = onException
  , retryFinish = finish
  } request = liftIO $ do
  result <- newEmptyMVar
  let complete = void . tryPutMVar result . Right
      onFailureException = void . tryPutMVar result . Left
      startAttempt state = request onSuccess (onFailure state) `catch` onFailureException
      onSuccess response = (handleSuccess (body response) >>= evaluate >>= complete) `catch` onFailureException
      onFailure state response =
        (do
          decision <- handleError state (ClientError response)
          case decision of
            Left nextState -> startAttempt nextState
            Right raw -> evaluate raw >>= complete)
        `catch` onFailureException
  startAttempt initialState
  pure (ClientAsync result finish onException)
#endif

runClient :: MonadIO m => RetryPolicy m a result -> ClientRequest a -> m result
runClient policy request = awaitClient =<< runClientAsync policy request

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

awaitClient :: MonadIO m => ClientAsync m result -> m result
#ifdef VANILLA
awaitClient (ClientAsync asyncRequest finish onException) = do
  outcome <- liftIO (waitCatch asyncRequest)
  either onException finish outcome
#else
awaitClient (ClientAsync result finish onException) = do
  outcome <- liftIO (readMVar result)
  either onException finish outcome
#endif
