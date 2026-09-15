{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Servant.Multipart.API.Compat
  ( MultipartCompat
  ) where

import Servant.Multipart.API (MultipartResult)
import Data.Kind (Type)

#ifndef VANILLA
import Miso.FFI (Blob)
#endif

type MultipartCompat :: Type -> Type
type data MultipartCompat api

#ifdef VANILLA
type instance MultipartResult (MultipartCompat api) = MultipartResult api
#else
type instance MultipartResult (MultipartCompat api) = Blob
#endif
