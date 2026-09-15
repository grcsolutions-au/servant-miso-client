{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Multipart.Client.Compat
  ( genBoundary
  )
where

#ifdef VANILLA
import Servant.Multipart.Client (genBoundary)
#else
import Control.Monad (forM_, void)
import Data.Proxy (Proxy(Proxy))
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
  ( MultipartData
  , MultipartForm'
  , ToMultipart (toMultipart)
  , fdFileName
  , fdInputName
  , fdPayload
  , files
  , iName
  , iValue
  , inputs
  )
import Servant.Multipart.API.Compat (Tmp)
#endif

#ifndef VANILLA
genBoundary :: IO ()
genBoundary = pure ()

multipartBody :: MultipartData Tmp -> IO JSVal
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
  , ToMultipart Tmp a
  ) => HasClient (MultipartForm' mods Tmp a :> api) where
  type ClientType (MultipartForm' mods Tmp a :> api) = ((), a) -> ClientType api
  toClientInternal _ req (_, body) =
    toClientInternal
      (Proxy @api)
      (req { _reqBody = Just (multipartBody (toMultipart @Tmp body)) })
#endif
