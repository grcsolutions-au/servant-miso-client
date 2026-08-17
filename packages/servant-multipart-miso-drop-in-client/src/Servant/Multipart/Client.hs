{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Multipart.Client
#ifdef VANILLA
  ( module NativeMultipartClient
  )
#else
  ( genBoundary
  )
#endif
where

#ifdef VANILLA
import "servant-multipart-client" Servant.Multipart.Client as NativeMultipartClient
#else
import Data.Proxy (Proxy(Proxy))
import Control.Monad (forM_, void)
import Miso (JSVal)
import Miso.DSL (jsg, new, toJSVal)
import Miso.FFI (callFunction)
import Miso.String (MisoString, ms)
import Servant.API ((:>))
import Servant.Miso.Client
  ( HasClient (ClientType, toClientInternal)
  , Request (_reqBody)
  )
import Servant.Multipart.API
  ( FileData (FileData)
  , Input (Input)
  , JsBlob
  , MultipartData (MultipartData)
  , MultipartForm'
  , ToMultipart (toMultipart)
  , Tmp
  )
#endif

#ifndef VANILLA
genBoundary :: IO ()
genBoundary = pure ()

multipartBody :: MultipartData Tmp -> IO JSVal
multipartBody (MultipartData inputParts fileParts) = do
  formData <- new (jsg "FormData") ([] :: [MisoString])
  forM_ inputParts $ \(Input name fieldValue) ->
    void $ callFunction formData "append" (ms name, ms fieldValue)
  forM_ fileParts $ \(FileData name fileName _ payload) -> do
    payloadBytes <- toJSVal payload
    file <- new (jsg "File") ([payloadBytes], ms fileName)
    void $ callFunction formData "append" (ms name, file)
  pure formData

instance
  ( HasClient api
  , ToMultipart JsBlob a
  ) => HasClient (MultipartForm' mods JsBlob a :> api) where
  type ClientType (MultipartForm' mods JsBlob a :> api) = ((), a) -> ClientType api
  toClientInternal _ req (_, body) =
    toClientInternal
      (Proxy @api)
      (req { _reqBody = Just (multipartBody (toMultipart @JsBlob body)) })
#endif
