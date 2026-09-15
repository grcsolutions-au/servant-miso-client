{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Multipart.Server.Compat () where

import Data.Proxy (Proxy(Proxy))
import Servant.Multipart (MultipartBackend(..))
import Servant.Multipart.API.Compat (MultipartCompat)

instance forall tag. MultipartBackend tag => MultipartBackend (MultipartCompat tag) where
  type MultipartBackendOptions (MultipartCompat tag) = MultipartBackendOptions tag
  backend _ = backend (Proxy @tag)
  defaultBackendOptions _ = defaultBackendOptions (Proxy @tag)
