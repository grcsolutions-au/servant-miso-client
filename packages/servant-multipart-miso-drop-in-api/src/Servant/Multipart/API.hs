{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PackageImports #-}

module Servant.Multipart.API
  ( module Reexported
#ifndef VANILLA
  , Tmp
  , Mem
  , JsBlob
#endif
  ) where

#ifdef VANILLA
import "servant-multipart-api" Servant.Multipart.API as Reexported
#else
import "servant-multipart-api" Servant.Multipart.API as Reexported hiding (Tmp, Mem)
import Miso.FFI (Blob)

type data JsBlob

type instance MultipartResult JsBlob = Blob

type Tmp = JsBlob

type Mem = JsBlob
#endif
