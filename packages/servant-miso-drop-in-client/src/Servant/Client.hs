{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{-| Servant / Miso drop-in API compatibility layer

By depending on 'servant-miso-drop-in-client' instead of 'servant-client' 
and 'servant-miso-client' directly, in theory you can write API interfaces 
that build happily with the same source code for both native and Miso builds 
without changing your existing API definitions.

In practice this overlay is probably incomplete at the moment.
-}
module Servant.Client
(
  ClientAsync
  , clientWithEnv
  , consoleError
  , consoleLog
  , Manager
  , mkBaseUrl
  , newManager
  , runClientMAsync
  , await
#ifdef VANILLA
  , module NativeServantClientExport
#else
  , BaseUrl(..)
  , Client
  , ClientM
  , ClientError(..)
  , ClientEnv
  , HasClient (..)
  , Scheme(..)
  , mkClientEnv
#endif
) where

import Data.Proxy (Proxy)
import Data.Text (Text)
#ifdef VANILLA
import Control.Concurrent.Async (Async, async, wait)
import qualified Data.Text.IO as TextIO
import qualified Network.HTTP.Client as HttpClient
import System.IO (stderr)
import qualified "servant-client" Servant.Client as NativeServantClient
import "servant-client" Servant.Client as NativeServantClientExport
#else
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Data.Kind (Type)
import qualified Miso.FFI as MisoFFI
import Miso.FFI (Response(..))
import Miso.String (MisoString, ms)
import Servant.Miso.Client
  ( ClientType
  , HasClient (..)
  , toClient
  )
#endif

#ifdef VANILLA
type Manager = HttpClient.Manager
#else
newtype Manager = Manager ()
#endif

#ifdef VANILLA
type ClientAsync a = Async a
#else
newtype ClientAsync a = ClientAsync (MVar a)
#endif

#ifndef VANILLA
type data ClientM :: Type -> Type

newtype ClientError = ClientError (Response MisoString)

newtype BaseUrl = BaseUrl MisoString

newtype ClientEnv = ClientEnv BaseUrl

data Scheme
  = Http
  | Https

type family Client (clientM :: Type -> Type) api :: Type where
  Client ClientM api = ClientType api
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
newManager = HttpClient.newManager HttpClient.defaultManagerSettings
#else
newManager = pure (Manager ())
#endif

mkBaseUrl
  :: Scheme
  -> String
  -> Int
  -> String
  -> BaseUrl
#ifdef VANILLA
mkBaseUrl = BaseUrl
#else
mkBaseUrl scheme host port path =
  BaseUrl . ms $ schemePrefix scheme <> "//" <> host <> ":" <> show port <> path
  where
    schemePrefix Http = "http:"
    schemePrefix Https = "https:"
#endif

#ifndef VANILLA
mkClientEnv :: Manager -> BaseUrl -> ClientEnv
mkClientEnv _ = ClientEnv
#endif

runClientMAsync
#ifdef VANILLA
  :: (ClientEnv, ClientM a)
  -> IO (ClientAsync (Either ClientError a))
runClientMAsync (env, request) =
  async (NativeServantClient.runClientM request env)
#else
  :: ((Response a -> IO ()) -> (Response MisoString -> IO ()) -> IO ())
  -> IO (ClientAsync (Either ClientError a))
runClientMAsync request = do
  result <- newEmptyMVar
  request
    (\response -> putMVar result (Right (body response)))
    (\response -> putMVar result (Left (ClientError response)))
  pure (ClientAsync result)
#endif

clientWithEnv
#ifdef VANILLA
  :: ( HasClient ClientM api
     , Functor f
     , Client ClientM api ~ f a
     )
  => ClientEnv
  -> Proxy api
  -> f (ClientEnv, a)
clientWithEnv env api = (\x -> (env, x)) <$> NativeServantClient.client api
#else
  :: HasClient api
  => ClientEnv
  -> Proxy api
  -> Client ClientM api
clientWithEnv (ClientEnv (BaseUrl url)) api = toClient url api
#endif

await :: ClientAsync a -> IO a
#ifdef VANILLA
await = wait
#else
await (ClientAsync result) = takeMVar result
#endif
