-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Servant.API.ContentTypes.DerivingVia 
( MimeVia(..)
) where

import Data.Coerce (coerce)
import Data.Proxy (Proxy(Proxy))
#ifdef VANILLA
import Servant.API
  (Accept, MimeRender(mimeRender), MimeUnrender(mimeUnrender))
#else
import Servant.API
  (Accept, MimeRender(mimeRender), MimeUnrender(mimeUnrender, mimeUnrenderType))
#endif

{-| A newtype to derive MIME instances via an existing base MIME type.

If you want the serialisation properties of an existing MIME type but
just a new content type string, just define 'Accept' and 'MimeVia' 
will give you the required 'MimeRender' and 'MimeUnrender' instances. 

See 'Servant.API.ContentTypes.Extra.JavaScript' as an example.
-}
newtype MimeVia base new a = MimeVia a

instance
  (Accept new, MimeRender base a)
    => MimeRender new (MimeVia base new a) where
  mimeRender _ = coerce $ mimeRender @base @a (Proxy :: Proxy base)

instance
  (Accept new, MimeUnrender base a)
    => MimeUnrender new (MimeVia base new a) where
#ifndef VANILLA
  -- We define it like this without arguments so it just
  mimeUnrenderType _ _ = mimeUnrenderType (Proxy :: Proxy base) (Proxy :: Proxy a)
#endif
  mimeUnrender _ = coerce $ mimeUnrender @base @a (Proxy :: Proxy base)
