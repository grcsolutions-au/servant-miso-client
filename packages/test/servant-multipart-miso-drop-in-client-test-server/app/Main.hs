module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (pathInfo, responseLBS)
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

retryServer :: MVar (Map Int Int) -> Server RetryAPI
retryServer attempts failures = do
    attempt <- liftIO $ modifyMVar attempts $ \counts -> do
      let next = Map.findWithDefault 0 failures counts + 1
      pure (Map.insert failures next counts, next)
    if attempt <= failures then throwError err503 else pure attempt

app :: IO Application
app = do
    attempts <- newMVar Map.empty
    let api = serve (Proxy @TestAPI) (uploadServer :<|> retryServer attempts)
    pure $ \request sendResponse ->
      if pathInfo request == ["malformed-json"]
        then sendResponse (responseLBS status200 [(hContentType, "application/json")] "not-json")
        else api request sendResponse

main :: IO ()
main = app >>= run serverPort

serverPort :: Int
serverPort = 8090
