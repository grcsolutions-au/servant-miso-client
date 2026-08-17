module Servant.Multipart.Miso.Test.UploadTypes
  ( UploadForm(..)
  , UploadAck(..)
  , UploadAPI
  , attachmentChecksum
  , testPort
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Word (Word8)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Numeric (showHex)
import qualified Miso.JSON as MisoJSON
import qualified Crypto.Hash.MD5 as MD5
import Servant.API (JSON, Post, (:>))
import Servant.Multipart.API
  ( FileData(..)
  , FromMultipart(..)
  , Input(..)
  , MultipartData(..)
  , MultipartForm
  , Tmp
  , ToMultipart(..)
  )

data UploadForm = UploadForm
  { title :: Text
  , description :: Text
  , attachmentFileName :: Text
  , attachmentContents :: FileData Tmp
  }
  deriving stock (Generic)

data UploadAck = UploadAck
  { receivedTitle :: Text
  , receivedDescription :: Text
  , receivedAttachmentFileName :: Text
  , receivedAttachmentChecksum :: Text
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON UploadAck where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance Aeson.ToJSON UploadAck where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance MisoJSON.FromJSON UploadAck where
  parseJSON = MisoJSON.genericParseJSON MisoJSON.defaultOptions

instance MisoJSON.ToJSON UploadAck where
  toJSON = MisoJSON.genericToJSON MisoJSON.defaultOptions

type UploadAPI = "upload" :> MultipartForm Tmp UploadForm :> Post '[JSON] UploadAck

testPort :: Int
testPort = 8090

instance ToMultipart Tmp UploadForm where
  toMultipart UploadForm{..} = MultipartData
    [ Input "title" title
    , Input "description" description
    ]
    [ attachmentContents
    ]

instance FromMultipart Tmp UploadForm where
  fromMultipart multipartData = do
    title <- lookupField "title" multipartData
    description <- lookupField "description" multipartData
    attachmentContents <- lookupFile "attachment" multipartData
    let attachmentFileName = fdFileName attachmentContents
    pure UploadForm{..}

lookupField :: Text -> MultipartData tag -> Either String Text
lookupField fieldName (MultipartData inputs _) =
  case [ value | Input name value <- inputs, name == fieldName ] of
    value : _ -> Right value
    [] -> Left ("missing field: " <> show fieldName)

lookupFile :: Text -> MultipartData tag -> Either String (FileData tag)
lookupFile fieldName (MultipartData _ files) =
  case [ file | file@FileData{fdInputName = name} <- files, name == fieldName ] of
    file : _ -> Right file
    [] -> Left ("missing file: " <> show fieldName)

attachmentChecksum :: LBS8.ByteString -> Text
attachmentChecksum = Text.pack . concatMap byteToHex . BS.unpack . MD5.hashlazy
  where
    byteToHex :: Word8 -> String
    byteToHex byte =
      case showHex byte "" of
        [digit] -> ['0', digit]
        digits -> digits