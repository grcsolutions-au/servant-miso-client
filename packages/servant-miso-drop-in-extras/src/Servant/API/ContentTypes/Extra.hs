{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.API.ContentTypes.Extra
( JavaScript
) where

import Network.HTTP.Media (MediaType, (//), (/:))
import Data.List.NonEmpty (NonEmpty((:|)))
import Servant.API (MimeRender, MimeUnrender, Accept(contentTypes), PlainText)
import Servant.API.ContentTypes.DerivingVia (MimeVia(MimeVia))
import Miso.String.Compat (MisoString)

{-| JavaScript content type for Servant APIs

Just wraps plain text but with the JavaScript content type string.
-}
data JavaScript

javascriptContentType :: NonEmpty MediaType
javascriptContentType = (base /: ("charset", "utf-8")) :| [base] where
  base = "text" // "javascript"

instance Accept JavaScript where
  contentTypes = const javascriptContentType

deriving via (MimeVia PlainText JavaScript MisoString)
  instance MimeRender JavaScript MisoString
deriving via (MimeVia PlainText JavaScript MisoString)
  instance MimeUnrender JavaScript MisoString
