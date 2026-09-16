module Main where

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import qualified Data.Text as Text
import Servant
import Servant.Multipart ()
import Servant.Multipart.API (fdPayload)
import Servant.Multipart.Server.Compat ()
import Servant.Multipart.Miso.Test.UploadTypes

uploadServer :: Server UploadAPI
uploadServer UploadForm{..} = do
    receivedAttachmentContents <- liftIO (attachmentContentsFile (fdPayload attachmentContents))
    pure UploadAck
      { receivedTitle = title
      , receivedDescription = description
      , receivedAttachmentFileName = attachmentFileName
      , receivedAttachmentContents = receivedAttachmentContents
      }

attachmentContentsFile :: FilePath -> IO Text
attachmentContentsFile filePath = Text.pack . LBS8.unpack <$> LBS8.readFile filePath

app :: Application
app = serve (Proxy @UploadAPI) uploadServer

main :: IO ()
main = run testPort app
