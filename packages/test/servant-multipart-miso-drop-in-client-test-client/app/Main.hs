{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT(..), runContT)
import Data.List (isInfixOf)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Servant.API.Compat (Capture, Get, JSON, (:>), (:<|>)(..))
import Servant.Multipart.API (FileData(FileData), MultipartResult, Tmp)
import Servant.Multipart.API.Compat (MultipartCompat)
import Servant.Multipart.Client.Compat (withBoundary)
import Servant.Multipart.Miso.Test.UploadTypes
import Servant.Client.Compat (consoleError, consoleLog)
import qualified Servant.Client.Compat as Client
import qualified System.Exit as Exit

#ifdef VANILLA
import System.Directory (getTemporaryDirectory)
import System.IO (hClose, openTempFile)
#else
import Servant.API.Compat (NoContentVerb, StdMethod(GET))
import Miso.DSL (jsg, new)
import Miso.FFI (Blob(..))
import Miso.String (MisoString, ms)
#endif

type TestM = ContT () IO

main :: IO ()
main = runContT runClientTest pure

#ifdef wasm32_HOST_ARCH
foreign export javascript "hs_start" main :: IO ()
#endif

fixtureFileContents :: LBS8.ByteString
fixtureFileContents = LBS8.pack "multipart file contents"

expectedAck :: UploadAck
expectedAck = UploadAck "alpha" "beta" "fixture.txt" (pack (LBS8.unpack fixtureFileContents))

type WrongResultAPI = "retry" :> Capture "failures" Int :> Get '[JSON] UploadAck

#ifndef VANILLA
type MalformedJsonAPI = "malformed-json" :> Get '[JSON] Int

type NoContentFailAPI = "retry" :> Capture "failures" Int :> NoContentVerb 'GET
#endif

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

uploadForm :: MultipartResult (MultipartCompat Tmp) -> UploadForm
uploadForm payload = UploadForm
  { title = "alpha"
  , description = "beta"
  , attachmentFileName = "fixture.txt"
  , attachmentContents = FileData "attachment" "fixture.txt" "text/plain" payload
  }

runClientTest :: TestM ()
runClientTest = do
  upload <- liftIO expectedUpload
  requestBody <- liftIO (withBoundary upload)
  manager <- liftIO Client.newManager
  let baseUrl = Client.mkBaseUrl Client.Http "127.0.0.1" testPort ""
      clientEnv = Client.mkClientEnv manager baseUrl
      uploadRequest :<|> retryRequest = Client.clientWithEnv clientEnv (Proxy @TestAPI)
      echoEnv = Client.mkClientEnv manager (Client.mkBaseUrl Client.Http "127.0.0.1" 8081 "")
      echoGet :<|> echoStatus = Client.clientWithEnv echoEnv (Proxy @EchoAPI)
      wrongResult = Client.clientWithEnv clientEnv (Proxy @WrongResultAPI)
  echoResponse <- succeed echoGet
  liftIO $ expect ("/get" `isInfixOf` unpack (url echoResponse)) "echo server did not return the request URL"
  expectHttpError 418 (echoStatus 418)
  response <- succeed (uploadRequest requestBody)
  liftIO $ expect (response == expectedAck)
    ("unexpected response: " <> pack (show response))

  expectHttpError 503 (retryRequest 2)
  expectHttpError 503 (retryRequest 2)
  retried <- succeed (retryRequest 2)
  liftIO $ expect (retried == 3) "separate request did not succeed after two failures"
  expectHttpError 503 (retryRequest 20)
  expectInvalid200 (wrongResult 0)
#ifndef VANILLA
  let malformedJson = Client.clientWithEnv clientEnv (Proxy @MalformedJsonAPI)
  expectInvalid200 malformedJson
  let noContentRequest = Client.clientWithEnv clientEnv (Proxy @NoContentFailAPI)
  expectHttpError 503 (noContentRequest 1)
#endif
  let unavailable = Client.clientWithEnv
        (Client.mkClientEnv manager (Client.mkBaseUrl Client.Http "127.0.0.1" (testPort + 2) ""))
        (Proxy @TestAPI)
      _ :<|> unavailableRetry = unavailable
  connectionResult <- request (unavailableRetry 0)
  liftIO $ case connectionResult of
    Left err -> do
      expect (Client.clientErrorStatus err == Nothing) "connection error unexpectedly had a status"
      expect (not (Text.null (Client.clientErrorMessage err))
        && Client.clientErrorMessage err /= "Request failed") "missing connection error message"
      case Client.clientErrorException err of
        Nothing -> pure ()
        Just exception ->
          expect (not (null (show exception))) "missing underlying transport exception"
    Right _ -> failTest "connection failure returned a successful result"
  liftIO $ consoleLog "SUCCESS"

request :: Client.ClientRequest a -> TestM (Either Client.ClientError a)
request req = ContT $ \k -> Client.sendRequest req (k . Right) (k . Left)

succeed :: Client.ClientRequest a -> TestM a
succeed req = do
  outcome <- request req
  case outcome of
    Left err -> liftIO (failTest ("client request failed: " <> Client.clientErrorMessage err))
    Right value -> pure value

expectHttpError :: Int -> Client.ClientRequest a -> TestM ()
expectHttpError status req = do
  outcome <- request req
  liftIO $ case outcome of
    Left err -> do
      expect (Client.clientErrorStatus err == Just status) ("unexpected HTTP status, expected " <> pack (show status))
      expect (not (Text.null (Client.clientErrorMessage err))) "missing HTTP error message"
    Right _ -> failTest ("expected HTTP " <> pack (show status))

expectInvalid200 :: Client.ClientRequest a -> TestM ()
expectInvalid200 req = do
  outcome <- request req
  liftIO $ case outcome of
    Left err -> do
      expect (Client.clientErrorStatus err == Just 200) "malformed 200 response lost its status"
      expect (not (Text.null (Client.clientErrorMessage err))) "missing decode error"
    Right _ -> failTest "expected malformed 200 response"

expect :: Bool -> Text -> IO ()
expect condition message = unless condition (failTest message)

failTest :: Text -> IO a
failTest message = consoleError ("ERROR: " <> message) >> consoleError "ERROR" >> Exit.exitFailure