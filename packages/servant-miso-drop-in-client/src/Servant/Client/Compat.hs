{-# LANGUAGE CPP #-}

module Servant.Client.Compat
  ( BaseUrl
  , Client
  , ClientAsync
  , ClientEnv
  , ClientError
  , ClientRequest
  , Manager
  , Scheme(..)
  , awaitClient
  , clientWithEnv
  , consoleError
  , consoleLog
  , mkBaseUrl
  , mkClientEnv
  , newManager
  , runClient
  , runClientAsync
  ) where


import Data.Proxy (Proxy)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO, liftIO)

#ifdef VANILLA
import Control.Concurrent.Async (Async, async, wait)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text.IO as TextIO
import qualified Network.HTTP.Client as HttpClient
import System.IO (stderr)
import qualified Servant.Client as NativeServantClient
#else
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Exception (Exception(displayException), throwIO)
import Control.Monad (unless)
import qualified Miso.FFI as MisoFFI
import Miso.FFI (Response(body))
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

--newtype RetryPolicy a = RetryPolicy s ((ClientError, s) -> Maybe b) (a -> b)

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
  RetryPolicy :: acc -> (ClientError -> Either acc (m b)) -> (a -> m b) -> RetryPolicy m a b

applyRetryPolicy
  :: MonadIO m
  => RetryPolicy m a b
  -> IO (Either ClientError a)
  -> m b
applyRetryPolicy (RetryPolicy initialAcc handleError handleSuccess) action = go initialAcc where
  go acc = do
    result <- liftIO action
    case result of
      Left err -> case handleError err of
        Left newAcc -> go newAcc
        Right b -> b
      Right a -> handleSuccess a

#ifdef VANILLA
runClientAsync
  :: forall a. ClientRequest a
  -> IO (ClientAsync (Either ClientError a))
runClientAsync (ClientRequest request) = ClientAsync <$> async request
#else

data MisoRequestCallbackException = MisoRequestCallbackCalledMoreThanOnce
  deriving stock (Show)

instance Exception MisoRequestCallbackException where
  displayException MisoRequestCallbackCalledMoreThanOnce =
    "Miso request code called its callback more than once. This should never happen. This indicates a bug in the Miso request code."

runClientAsync request = do
  result <- newEmptyMVar
  let
    putMVarOrThrow :: Either ClientError a -> IO ()
    putMVarOrThrow value = do
      success <- tryPutMVar result value
      unless success $ throwIO MisoRequestCallbackCalledMoreThanOnce
  request
    -- Assuming `request` is not buggy it should only call one of these callbacks
    -- and only once. That's why we throw (above) if 'tryPutMVar' ever fails.
    -- It never should fail so so if it does it's a bug.
    (\response -> putMVarOrThrow (Right (body response)))
    (\response -> putMVarOrThrow (Left (ClientError response)))
  pure (ClientAsync result)
#endif

runClient :: ClientRequest a -> IO (Either ClientError a)
#ifdef VANILLA
runClient (ClientRequest request) = request
#else
runClient request = await =<< runClientAsync request
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

awaitClient :: ClientAsync a -> IO a
#ifdef VANILLA
awaitClient (ClientAsync asyncRequest) = wait asyncRequest
#else
awaitClient (ClientAsync result) = readMVar result
#endif
