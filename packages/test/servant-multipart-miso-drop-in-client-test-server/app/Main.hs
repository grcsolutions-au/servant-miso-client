module Main where

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Servant
import Servant.Multipart ()
import Servant.Multipart.API (FileData(..))
import Servant.Multipart.Miso.Test.UploadTypes

uploadServer :: Server UploadAPI
uploadServer UploadForm{..} = do
    receivedAttachmentChecksum <- liftIO (attachmentChecksumFile (fdPayload attachmentContents))
    pure UploadAck
      { receivedTitle = title
      , receivedDescription = description
      , receivedAttachmentFileName = attachmentFileName
      , receivedAttachmentChecksum = receivedAttachmentChecksum
      }

attachmentChecksumFile :: FilePath -> IO Text
attachmentChecksumFile filePath = attachmentChecksum <$> LBS.readFile filePath

app :: Application
app = serve (Proxy @UploadAPI) uploadServer

main :: IO ()
main = run testPort app
