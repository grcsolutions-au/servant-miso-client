{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Proxy (Proxy(..))
import Data.Text (pack)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Servant.Multipart.API (FileData(..), MultipartResult, Tmp)
import Servant.Multipart.Client (genBoundary)
import Servant.Multipart.Miso.Test.UploadTypes
import Servant.Client (consoleError, consoleLog)
import qualified Servant.Client as Client
import qualified System.Exit as Exit

#ifdef VANILLA
import System.Directory (getTemporaryDirectory)
import System.IO (hClose, openTempFile)
#else
import Miso.DSL (jsg, new)
import Miso.FFI (Blob(..))
import Miso.String (MisoString, ms)
import Servant.Multipart.Client ()
#endif

main :: IO ()
main = runClientTest

#ifdef wasm32_HOST_ARCH
foreign export javascript "hs_start" main :: IO ()
#endif

fixtureFileContents :: LBS8.ByteString
fixtureFileContents = LBS8.pack "multipart file contents"

expectedAck :: UploadAck
expectedAck = UploadAck "alpha" "beta" "fixture.txt" (attachmentChecksum fixtureFileContents)

expectedUpload :: IO UploadForm
#ifdef VANILLA
expectedUpload = do
  temporaryDirectory <- getTemporaryDirectory
  (path, handle) <- openTempFile temporaryDirectory "servant-multipart-miso-fixture-"
  LBS8.hPut handle fixtureFileContents
  hClose handle
  pure (uploadForm path)
#else
expectedUpload = do
  attachmentPayload <- Blob <$> new (jsg "Blob") ([[ms (LBS8.unpack fixtureFileContents)]] :: [[MisoString]])
  pure (uploadForm attachmentPayload)
#endif

uploadForm :: MultipartResult Tmp -> UploadForm
uploadForm payload = UploadForm
  { title = "alpha"
  , description = "beta"
  , attachmentFileName = "fixture.txt"
  , attachmentContents = FileData "attachment" "fixture.txt" "text/plain" payload
  }

runClientTest :: IO ()
runClientTest = do
  upload <- expectedUpload
  manager <- Client.newManager
  let baseUrl = Client.mkBaseUrl Client.Http "127.0.0.1" testPort ""
      clientEnv = Client.mkClientEnv manager baseUrl
  boundary <- genBoundary
  let request = Client.clientWithEnv clientEnv (Proxy @UploadAPI) (boundary, upload)
  asyncRequest <- Client.runClientMAsync request
  result <- Client.await asyncRequest
  case result of
    Right response
      | response == expectedAck ->
          consoleLog "SUCCESS"
      | otherwise ->
          consoleError ("ERROR: unexpected response: " <> pack (show response))
            >> Exit.exitFailure
    Left _ ->
      consoleError "ERROR: client request failed" >> Exit.exitFailure