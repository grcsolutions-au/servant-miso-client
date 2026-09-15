{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{-| Servant / Miso API compatibility layer.

This module combines the upstream Servant API with the Miso MIME classes
needed by browser clients.
-}
module Servant.API.Compat
  ( module Exported
  ) where

#ifdef VANILLA
import Servant.API as Exported
#else
import Servant.API as Exported hiding (MimeRender(..), MimeUnrender(..))
import Servant.Miso.Client as Exported (MimeRender(..), MimeUnrender(..))
#endif
