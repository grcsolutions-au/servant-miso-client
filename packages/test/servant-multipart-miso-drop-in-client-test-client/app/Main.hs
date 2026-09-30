{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (unless, void)
import Control.Exception (SomeException)
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
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
import Miso.FFI (Blob(..), Response(..))
import Miso.String (MisoString, ms)
#endif

main :: IO ()
main = do
  result <- runExceptT runClientTest
  case result of
    Left err ->
      failTest ("client request failed: " <> Client.clientErrorMessage err)
    Right () -> pure ()

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

runClientTest :: ExceptT Client.ClientError IO ()
runClientTest = do
  upload <- liftIO expectedUpload
  requestBody <- liftIO (withBoundary upload)
  manager <- liftIO Client.newManager
  let baseUrl = Client.mkBaseUrl Client.Http "127.0.0.1" testPort ""
      clientEnv = Client.mkClientEnv manager baseUrl
      uploadRequest :<|> retryRequest = Client.clientWithEnv clientEnv (Proxy @TestAPI)
      echoEnv = Client.mkClientEnv manager (Client.mkBaseUrl Client.Http "127.0.0.1" 8081 "")
      echoGet :<|> echoStatus = Client.clientWithEnv echoEnv (Proxy @EchoAPI)
  echoResponse <- runRequest echoGet
  liftIO $ expect ("/get" `isInfixOf` unpack (url echoResponse)) "echo server did not return the request URL"
  expectStatus 418 (runRequest (echoStatus 418))
  response <- runRequest (uploadRequest requestBody)
  liftIO $ expect (response == expectedAck)
    ("unexpected response: " <> pack (show response))

  expectStatus503 (runRequest (retryRequest 2))
  expectStatus503 (runRequest (retryRequest 2))
  retried <- runRequest (retryRequest 2)
  liftIO $ expect (retried == 3) "separate request did not succeed after two failures"
  repeatedWaits <- liftIO $ Client.withClientAsync (retryRequest 0) $ \pending -> do
    first <- Client.waitClient pending
    second <- Client.waitClient pending
    pure (first, second)
  liftIO $ case repeatedWaits of
    (Right first, Right second) ->
      expect (first == second) "waiting twice changed the client result"
    _ -> failTest "client request failed while checking repeated waits"
  expectStatus503 (runRequest (retryRequest 20))
  let wrongResult = Client.clientWithEnv clientEnv (Proxy @WrongResultAPI)
  expectInvalid200 (runRequest (wrongResult 0))
#ifndef VANILLA
  let malformedJson = Client.clientWithEnv clientEnv (Proxy @MalformedJsonAPI)
  expectInvalid200 (runRequest malformedJson)
  let noContentRequest = Client.clientWithEnv clientEnv (Proxy @NoContentFailAPI)
  expectStatus503 (runRequest (noContentRequest 1))
  let immediateRequest :: Client.ClientRequest Int
      immediateRequest onSuccess _ = onSuccess (Response (Just 200) mempty Nothing 1)
      throwingCallback :: Client.ClientRequest Int
      throwingCallback onSuccess _ = onSuccess (Response (Just 200) mempty Nothing (error "callback exception"))
      failedLaunch :: Client.ClientRequest Int
      failedLaunch _ _ = ioError (userError "request launch failed")
  immediate <- liftIO (Client.runClientSync immediateRequest)
  liftIO $ case immediate of
    Right 1 -> pure ()
    _ -> failTest "immediate callback result was not returned"
  expectException "callback exception" $ do
    result <- Client.runClientSync throwingCallback
    case result of
      Right value -> value `seq` pure ()
      Left _ -> pure ()
  expectException "request launch failed" (Client.runClientSync failedLaunch)
#endif
  let unavailable = Client.clientWithEnv
        (Client.mkClientEnv manager (Client.mkBaseUrl Client.Http "127.0.0.1" (testPort + 2) ""))
        (Proxy @TestAPI)
      _ :<|> unavailableRetry = unavailable
  connectionResult <- (Right <$> runRequest (unavailableRetry 0)) `catchE` (pure . Left)
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

runRequest :: Client.ClientRequest a -> ExceptT Client.ClientError IO a
runRequest request = do
  outcome <- liftIO (Client.withClientAsync request Client.waitClient)
  either throwE pure outcome

expectStatus503 :: ExceptT Client.ClientError IO a -> ExceptT Client.ClientError IO ()
expectStatus503 request =
  (void request >> liftIO (failTest "expected HTTP 503")) `catchE` \err ->
    liftIO $ do
      expect (Client.clientErrorStatus err == Just 503) "unexpected HTTP status"
      expect (not (Text.null (Client.clientErrorMessage err))) "missing HTTP error message"

expectStatus :: Int -> ExceptT Client.ClientError IO a -> ExceptT Client.ClientError IO ()
expectStatus status request =
  (void request >> liftIO (failTest ("expected HTTP " <> pack (show status)))) `catchE` \err ->
    liftIO $ expect (Client.clientErrorStatus err == Just status) "unexpected HTTP status"

expectInvalid200 :: ExceptT Client.ClientError IO a -> ExceptT Client.ClientError IO ()
expectInvalid200 request =
  (void request >> liftIO (failTest "expected malformed 200 response")) `catchE` \err ->
    liftIO $ do
      expect (Client.clientErrorStatus err == Just 200) "malformed 200 response lost its status"
      expect (not (Text.null (Client.clientErrorMessage err))) "missing decode error"

expectException :: Text -> IO a -> ExceptT Client.ClientError IO ()
expectException message action = do
  outcome <- liftIO (Catch.try (void action) :: IO (Either SomeException ()))
  liftIO $ case outcome of
    Left exception -> expect (unpack message `isInfixOf` show exception) "unexpected policy exception"
    Right () -> failTest "policy exception did not reach await"

expect :: Bool -> Text -> IO ()
expect condition message = unless condition (failTest message)

failTest :: Text -> IO a
failTest message = consoleError ("ERROR: " <> message) >> consoleError "ERROR" >> Exit.exitFailure