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
import Servant.Client.Core (HasClient(..), RunClient)
import Servant.Multipart.Client (genBoundary)
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
  , ToMultipart (toMultipart)
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
instance
  ( RunClient m
  , HasClient m api
  ) => HasClient m (MultipartCompat api) where
  type Client m (MultipartCompat api) = Client m api
  clientWithRoute pm _ =
    clientWithRoute pm (Proxy @api)
  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy @api)

instance
  ( HasClient m (MultipartForm' mods tag a :> api)
  ) => HasClient m (MultipartForm' mods (MultipartCompat tag) a :> api) where
  type Client m (MultipartForm' mods (MultipartCompat tag) a :> api) =
    Client m (MultipartForm' mods tag a :> api)
  clientWithRoute pm _ =
    clientWithRoute pm (Proxy @(MultipartForm' mods tag a :> api))
  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy @(MultipartForm' mods tag a :> api))

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
