{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Multipart.API.Compat
  ( Tmp
  , Mem
  ) where

#ifdef VANILLA
import Servant.Multipart.API (Mem, Tmp)
#else
import Miso.FFI (Blob)
import Servant.Multipart.API (MultipartResult)

type data JsBlob

type instance MultipartResult JsBlob = Blob

type Tmp = JsBlob

type Mem = JsBlob
#endif
