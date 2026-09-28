module Servant.Multipart.Miso.Test.UploadTypes
  ( UploadForm(..)
  , UploadAck(..)
  , UploadAPI
  , RetryAPI
  , TestAPI
  , testPort
  ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Miso.JSON as MisoJSON
import Servant.API.Compat (Capture, Get, JSON, Post, (:<|>), (:>))
import Servant.Multipart.API
  ( FileData
  , FromMultipart(..)
  , Input(Input)
  , MultipartData(MultipartData)
  , MultipartForm
  , ToMultipart(..)
  , Tmp
  , fdFileName
  , lookupInput
  , lookupFile
  )
import Servant.Multipart.API.Compat (MultipartCompat)

data UploadForm = UploadForm
  { title :: Text
  , description :: Text
  , attachmentFileName :: Text
  , attachmentContents :: FileData (MultipartCompat Tmp)
  }
  deriving stock (Generic)

data UploadAck = UploadAck
  { receivedTitle :: Text
  , receivedDescription :: Text
  , receivedAttachmentFileName :: Text
  , receivedAttachmentContents :: Text
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

type UploadAPI = "upload" :> MultipartForm (MultipartCompat Tmp) UploadForm :> Post '[JSON] UploadAck

type RetryAPI = "retry" :> Capture "failures" Int :> Get '[JSON] Int

type TestAPI = UploadAPI :<|> RetryAPI

testPort :: Int
testPort = 8090

instance ToMultipart (MultipartCompat Tmp) UploadForm where
  toMultipart UploadForm{..} = MultipartData
    [ Input "title" title
    , Input "description" description
    ]
    [ attachmentContents
    ]

instance FromMultipart (MultipartCompat Tmp) UploadForm where
  fromMultipart multipartData = do
    title <- lookupInput "title" multipartData
    description <- lookupInput "description" multipartData
    attachmentContents <- lookupFile "attachment" multipartData
    let attachmentFileName = fdFileName attachmentContents
    pure UploadForm{..}