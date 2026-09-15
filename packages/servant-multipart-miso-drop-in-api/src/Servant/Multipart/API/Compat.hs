{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Multipart.API.Compat
  ( module Reexported
#ifndef VANILLA
  , Tmp
  , Mem
  , JsBlob
#endif
  ) where

#ifdef VANILLA
import Servant.Multipart.API as Reexported
#else
import Miso.FFI (Blob)
import Servant.Multipart.API as Reexported hiding (Tmp, Mem)

type data JsBlob

type instance MultipartResult JsBlob = Blob

type Tmp = JsBlob

type Mem = JsBlob
#endif
