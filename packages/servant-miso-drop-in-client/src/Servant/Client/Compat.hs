{-# LANGUAGE CPP #-}

module Servant.Client.Compat
  ( BaseUrl
  , Client
  , ClientAsync
  , ClientEnv
  , ClientError(..)
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
import Control.Exception (SomeException, SomeAsyncException, evaluate, fromException, tryJust)
import Control.Monad.Catch (MonadThrow, throwM)

#ifdef VANILLA
import qualified Data.Text as Text
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

data ClientAsync m result where
#ifdef VANILLA
  ClientAsync :: Async raw -> (raw -> m result) -> ClientAsync m result
#else
  ClientAsync :: MVar (Either SomeException raw) -> (raw -> m result) -> ClientAsync m result
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
       , retryFinish :: raw -> m result
       }
    -> RetryPolicy m a result

noRetry
  :: (ClientError -> IO raw)
  -> (a -> IO raw)
  -> (raw -> m result)
  -> RetryPolicy m a result
noRetry onError retryOnSuccess retryFinish = RetryPolicy
  { retryInitialState = ()
  , retryOnError = \_ err -> Right <$> onError err
  , retryOnSuccess
  , retryFinish
  }

runClientAsync :: MonadIO m => RetryPolicy m a result -> ClientRequest a -> m (ClientAsync m result)
#ifdef VANILLA
runClientAsync RetryPolicy{..} (ClientRequest request) = liftIO $ do
  worker <- async (go retryInitialState)
  pure (ClientAsync worker retryFinish)
  where
    go state = do
      response <- tryJust synchronousException request
      case response of
        Left exception -> handleError state (RequestException exception)
        Right (Left err) -> handleError state err
        Right (Right value) -> retryOnSuccess value >>= evaluate
    handleError state err = do
          decision <- retryOnError state err
          case decision of
            Left nextState -> go nextState
            Right raw -> evaluate raw
#else

runClientAsync RetryPolicy{..} request = liftIO $ do
  result <- newEmptyMVar
  let complete = void . tryPutMVar result . Right
      onFailureException = void . tryPutMVar result . Left
      startAttempt state = (do
        launched <- tryJust synchronousException (request onSuccess (onFailure state))
        case launched of
          Left exception -> handleError state (RequestException exception)
          Right () -> pure ()) `catch` onFailureException
      onSuccess response = (retryOnSuccess (body response) >>= evaluate >>= complete) `catch` onFailureException
      onFailure state response = handleError state (fromBrowserClientError response) `catch` onFailureException
      handleError state err = do
        decision <- retryOnError state err
        case decision of
          Left nextState -> startAttempt nextState
          Right raw -> evaluate raw >>= complete
  startAttempt retryInitialState
  pure (ClientAsync result retryFinish)
#endif

synchronousException :: SomeException -> Maybe SomeException
synchronousException exception = case fromException exception :: Maybe SomeAsyncException of
  Just _ -> Nothing
  Nothing -> Just exception

runClient :: (MonadIO m, MonadThrow m) => RetryPolicy m a result -> ClientRequest a -> m result
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
      Bifunctor.first fromNativeClientError <$> NativeServantClient.runClientM request env
#else
clientWithEnv (ClientEnv (BaseUrl url)) = MisoClient.toClient url
#endif

awaitClient :: (MonadIO m, MonadThrow m) => ClientAsync m result -> m result
#ifdef VANILLA
awaitClient (ClientAsync asyncRequest finish) = do
  outcome <- liftIO (waitCatch asyncRequest)
  either throwM finish outcome
#else
awaitClient (ClientAsync result finish) = do
  outcome <- liftIO (readMVar result)
  either throwM finish outcome
#endif
