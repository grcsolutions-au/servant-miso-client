{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{-| Servant / Miso drop-in API compatibility layer

By depending on 'servant-miso-drop-in-api' instead of 'servant' and 
'miso' directly, in theory you can write API interfaces that build 
happily with the same source code for both native and Miso builds 
without changing your existing API definitions.

In practice this overlay is probably incomplete at the moment.
-}
module Servant.API
( module Exported
) where

#ifdef VANILLA
import "servant" Servant.API as Exported
#else
import "servant" Servant.API as Exported hiding (MimeRender(..), MimeUnrender(..))
import Servant.Miso.Client as Exported (MimeRender(..), MimeUnrender(..))
#endif
