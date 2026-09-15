{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Servant.Client.Compat
  ( BaseUrl
  , Client
  , ClientAsync
  , ClientEnv
  , ClientError
  , Manager
  , Scheme(..)
  , await
  , clientWithEnv
  , consoleError
  , consoleLog
  , mkBaseUrl
  , mkClientEnv
  , newManager
  , runClientMAsync
  ) where

import Data.Proxy (Proxy)
import Data.Text (Text)

#ifdef VANILLA
import Control.Concurrent.Async (Async, async, wait)
import qualified Data.Text.IO as TextIO
import qualified Network.HTTP.Client as HttpClient
import System.IO (stderr)
import qualified Servant.Client as NativeServantClient
#else
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryPutMVar, takeMVar)
import qualified Miso.FFI as MisoFFI
import Miso.FFI (Response(..))
import Miso.String (MisoString, ms)
import qualified Servant.Miso.Client as MisoClient
#endif

data Scheme
  = Http
  | Https

#ifdef VANILLA
newtype Manager = Manager HttpClient.Manager

newtype BaseUrl = BaseUrl NativeServantClient.BaseUrl

newtype ClientEnv = ClientEnv NativeServantClient.ClientEnv

newtype ClientAsync a = ClientAsync (Async a)

newtype ClientError = ClientError NativeServantClient.ClientError

newtype NativeRequest a = NativeRequest (IO (Either ClientError a))

type Client api = NativeServantClient.Client NativeRequest api
#else
newtype Manager = Manager ()

newtype BaseUrl = BaseUrl MisoString

newtype ClientEnv = ClientEnv BaseUrl

newtype ClientAsync a = ClientAsync (MVar a)

newtype ClientError = ClientError (Response MisoString)

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
newManager = pure (Manager ())
#endif

mkBaseUrl :: Scheme -> String -> Int -> String -> BaseUrl
#ifdef VANILLA
mkBaseUrl scheme host port path =
  BaseUrl $ NativeServantClient.BaseUrl
    (nativeScheme scheme)
    host
    port
    path
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

#ifdef VANILLA
nativeRunClientM
  :: NativeServantClient.ClientEnv
  -> NativeServantClient.ClientM a
  -> NativeRequest a
nativeRunClientM env request = do
  NativeRequest $ do
    result <- NativeServantClient.runClientM request env
    pure $ case result of
      Left error_ -> Left (ClientError error_)
      Right value -> Right value

runClientMAsync
  :: NativeRequest a
  -> IO (ClientAsync (Either ClientError a))
runClientMAsync (NativeRequest request) = ClientAsync <$> async request
#else
runClientMAsync
  :: ((Response a -> IO ()) -> (Response MisoString -> IO ()) -> IO ())
  -> IO (ClientAsync (Either ClientError a))
runClientMAsync request = do
  result <- newEmptyMVar
  request
    (\response -> do
      _ <- tryPutMVar result (Right (body response))
      pure ())
    (\response -> do
      _ <- tryPutMVar result (Left (ClientError response))
      pure ())
  pure (ClientAsync result)
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
  NativeServantClient.hoistClient api (nativeRunClientM env)
    (NativeServantClient.client api)
#else
clientWithEnv (ClientEnv (BaseUrl url)) api = MisoClient.toClient url api
#endif

await :: ClientAsync a -> IO a
#ifdef VANILLA
await (ClientAsync asyncRequest) = wait asyncRequest
#else
await (ClientAsync result) = takeMVar result
#endif
