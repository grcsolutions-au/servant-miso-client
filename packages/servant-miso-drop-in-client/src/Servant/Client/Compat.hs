{-# LANGUAGE CPP #-}

module Servant.Client.Compat
  ( BaseUrl
  , Client
  , ClientEnv
  , ClientError(..)
  , ClientRequest
  , Manager
  , RetryPolicy(..)
  , Scheme(..)
  , clientErrorStatus
  , clientWithEnv
  , consoleError
  , consoleLog
  , mkBaseUrl
  , mkClientEnv
  , newManager
  , noRetry
  , runClient
  ) where


import Data.Proxy (Proxy)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (SomeException, evaluate)
import qualified Control.Exception.Safe as Safe

#ifdef VANILLA
import qualified Data.Text as Text
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text.IO as TextIO
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Types.Status as HttpStatus
import System.IO (stderr)
import qualified Servant.Client as NativeServantClient
#else
import Control.Concurrent.MVar (newEmptyMVar, readMVar, tryPutMVar)
import qualified Control.Exception as Exception
import Data.IORef (atomicModifyIORef', newIORef)
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

#ifdef VANILLA
runClient :: MonadIO m => RetryPolicy m a result -> ClientRequest a -> m result
runClient RetryPolicy{..} (ClientRequest request) = do
  raw <- liftIO (go retryInitialState)
  retryFinish raw
  where
    go state = do
      response <- Safe.tryAny request
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
data BrowserState
  = BrowserNotStarted
  | BrowserAttempt Int
  | BrowserHandling Int
  | BrowserFinished
  | BrowserAbandoned

runClient :: MonadIO m => RetryPolicy m a result -> ClientRequest a -> m result
runClient RetryPolicy{..} request = do
  raw <- liftIO runBrowser
  retryFinish raw
  where
    runBrowser = Exception.mask $ \restore -> do
      state <- newIORef BrowserNotStarted
      result <- newEmptyMVar
      let finish generation outcome = do
            accepted <- atomicModifyIORef' state $ \current -> case current of
              BrowserHandling active | active == generation -> (BrowserFinished, True)
              _ -> (current, False)
            if accepted
              then do
                _ <- tryPutMVar result outcome
                pure ()
              else pure ()
          finishException exception = do
            accepted <- atomicModifyIORef' state $ \current -> case current of
              BrowserAttempt _ -> (BrowserFinished, True)
              BrowserHandling _ -> (BrowserFinished, True)
              _ -> (current, False)
            if accepted
              then do
                _ <- tryPutMVar result (Left exception)
                pure ()
              else pure ()
          supervise action = do
            outcome <- Exception.try action :: IO (Either SomeException ())
            case outcome of
              Left exception -> finishException exception
              Right () -> pure ()
          claim generation = atomicModifyIORef' state $ \current -> case current of
            BrowserAttempt active | active == generation -> (BrowserHandling generation, True)
            _ -> (current, False)
          callback generation action = supervise $ do
            accepted <- claim generation
            if accepted then action else pure ()
          startAttempt generation retryState = do
            started <- atomicModifyIORef' state $ \current -> case current of
              BrowserNotStarted | generation == 0 -> (BrowserAttempt generation, True)
              BrowserHandling previous | generation == previous + 1 -> (BrowserAttempt generation, True)
              _ -> (current, False)
            if not started
              then pure ()
              else supervise $ do
                launched <- Safe.tryAny $ request
                  (\response -> callback generation $ do
                    raw <- retryOnSuccess (body response) >>= evaluate
                    finish generation (Right raw))
                  (\response -> callback generation $
                    handleError generation retryState (fromBrowserClientError response))
                case launched of
                  Left exception -> callback generation $
                    handleError generation retryState (RequestException exception)
                  Right () -> pure ()
          handleError generation retryState err = do
            decision <- retryOnError retryState err
            case decision of
              Left nextState -> startAttempt (generation + 1) nextState
              Right raw -> evaluate raw >>= finish generation . Right
          abandon = atomicModifyIORef' state $ \current -> case current of
            BrowserFinished -> (current, ())
            BrowserAbandoned -> (current, ())
            _ -> (BrowserAbandoned, ())
      restore (startAttempt 0 retryInitialState) `Exception.onException` abandon
      outcome <- restore (readMVar result) `Exception.onException` abandon
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

