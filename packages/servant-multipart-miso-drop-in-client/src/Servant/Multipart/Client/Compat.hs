{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Multipart.Client.Compat
  ( genBoundary
  )
where

import Data.Proxy (Proxy(Proxy))
import Servant.API ((:>))
import Servant.Multipart.API.Compat (MultipartCompat)
import Servant.Multipart.API (MultipartForm')

#ifdef VANILLA
import qualified Data.ByteString.Lazy as LBS
import Servant.Client.Core (HasClient(..))
import Servant.Multipart.API (ToMultipart)
import Servant.Multipart.Client
  ( MultipartClient(..)
  , genBoundary
  , genericClientWithRoute
  , genericHoistClientMonad
  )
#else
import Control.Monad (forM_, void)
import Miso (JSVal)
import Miso.DSL (jsg, new, toJSVal)
import Miso.FFI (callFunction)
import Miso.String (MisoString, ms)
import Servant.Miso.Client
  ( HasClient (ClientType, toClientInternal)
  , Request (_reqBody)
  )
import Servant.Multipart.API
  ( MultipartData
  , ToMultipart(toMultipart)
  , fdFileName
  , fdInputName
  , fdPayload
  , files
  , iName
  , iValue
  , inputs
  )
#endif

#ifdef VANILLA
instance MultipartClient tag => MultipartClient (MultipartCompat tag) where
  loadFile _ = loadFile (Proxy @tag)

instance
  ( MultipartClient tag
  , ToMultipart (MultipartCompat tag) a
  , HasClient m api
  ) => HasClient m (MultipartForm' mods (MultipartCompat tag) a :> api) where
  type Client m (MultipartForm' mods (MultipartCompat tag) a :> api) =
    (LBS.ByteString, a) -> Client m api
  clientWithRoute = genericClientWithRoute
  hoistClientMonad = genericHoistClientMonad

#else
data FakeBoundary = FakeBoundary

genBoundary :: IO FakeBoundary
genBoundary = pure FakeBoundary

multipartBody :: MultipartData (MultipartCompat tag) -> IO JSVal
multipartBody multipartData = do
  formData <- new (jsg "FormData") ([] :: [MisoString])
  forM_ (inputs multipartData) $ \inputPart ->
    void $ callFunction formData "append" (ms (iName inputPart), ms (iValue inputPart))
  forM_ (files multipartData) $ \filePart -> do
    payloadBytes <- toJSVal (fdPayload filePart)
    file <- new (jsg "File") ([payloadBytes], ms (fdFileName filePart))
    void $ callFunction formData "append" (ms (fdInputName filePart), file)
  pure formData

instance
  ( HasClient api
  , ToMultipart (MultipartCompat tag) a
  ) => HasClient (MultipartForm' mods (MultipartCompat tag) a :> api) where
  type ClientType (MultipartForm' mods (MultipartCompat tag) a :> api) = (FakeBoundary, a) -> ClientType api
  toClientInternal _ req (_, body) =
    toClientInternal
      (Proxy @api)
      (req { _reqBody = Just (multipartBody (toMultipart @(MultipartCompat tag) body)) })
#endif
