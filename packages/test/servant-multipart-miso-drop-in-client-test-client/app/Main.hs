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
import Data.IORef (modifyIORef', newIORef, readIORef)
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
      failTest ("client request failed with HTTP status " <> pack (show (Client.clientErrorStatus err)))
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

retryExcept :: Client.RetryPolicy (ExceptT Client.ClientError IO) a a
retryExcept = Client.RetryPolicy
  { Client.retryInitialState = 0 :: Int
  , Client.retryOnError = handleError
  , Client.retryOnSuccess = pure . Right
  , Client.retryFinish = either throwE pure
  }
  where
    handleError retries err =
      pure $ if retries < 2 && Client.clientErrorStatus err == Just 503
        then Left (retries + 1)
        else Right (Left err)

runClientTest :: ExceptT Client.ClientError IO ()
runClientTest = do
  upload <- liftIO expectedUpload
  requestBody <- liftIO (withBoundary upload)
  manager <- liftIO Client.newManager
  let baseUrl = Client.mkBaseUrl Client.Http "127.0.0.1" testPort ""
      clientEnv = Client.mkClientEnv manager baseUrl
      uploadRequest :<|> retryRequest = Client.clientWithEnv clientEnv (Proxy @TestAPI)
      noRetry = Client.noRetry (pure . Left) (pure . Right) (either throwE pure)
  asyncRequest <- Client.runClientAsync noRetry (uploadRequest requestBody)
  response <- Client.awaitClient asyncRequest
  liftIO $ expect (response == expectedAck)
    ("unexpected response: " <> pack (show response))

  retried <- Client.runClientAsync retryExcept (retryRequest 2)
  first <- Client.awaitClient retried
  second <- Client.awaitClient retried
  liftIO $ expect (first == 3 && second == 3) "retry result changed across awaits"
  nextAttempt <- Client.runClient noRetry (retryRequest 2)
  liftIO $ expect (nextAttempt == 4) "awaiting repeated the HTTP request"

  failed <- Client.runClientAsync retryExcept (retryRequest 20)
  expectStatus503 (Client.awaitClient failed)
  expectStatus503 (Client.awaitClient failed)
  expectStatus503 (Client.runClient retryExcept (retryRequest 20))
  let diagnostic = Client.RetryPolicy
        { Client.retryInitialState = 0 :: Int
        , Client.retryOnError = \retries err -> pure $ if retries < 1
            then Left (retries + 1)
            else Right (retries + 1, Client.clientErrorStatus err)
        , Client.retryOnSuccess = \value -> pure (value, Nothing)
        , Client.retryFinish = pure
        } :: Client.RetryPolicy (ExceptT Client.ClientError IO) Int (Int, Maybe Int)
  stopped <- Client.runClient diagnostic (retryRequest 21)
  liftIO $ expect (stopped == (2, Just 503)) "policy did not use retry state and error"

  expectStatus503 (Client.runClient noRetry (retryRequest 22))
  direct <- Client.runClient noRetry (retryRequest 0)
  liftIO $ expect (direct == 1) "noRetry success failed"
  converted <- Client.runClient
    (Client.noRetry (\_ -> pure "failed") (pure . show)
      pure)
    (retryRequest 0)
  liftIO $ expect (converted == "2") "policy did not change the result type"
  let unexpected = Client.RetryPolicy
        { Client.retryInitialState = ()
        , Client.retryOnError = \_ _ -> fail "retry decision failed"
        , Client.retryOnSuccess = pure . show
        , Client.retryFinish = pure
        } :: Client.RetryPolicy (ExceptT Client.ClientError IO) Int String
  failedDecision <- Client.runClientAsync unexpected (retryRequest 23)
  expectThrown "retry decision failed" (Client.awaitClient failedDecision)
  expectThrown "retry decision failed" (Client.awaitClient failedDecision)
  expectThrown "retry decision failed" (Client.runClient unexpected (retryRequest 24))
  let failedSuccess = Client.noRetry (pure . Left) (\_ -> ioError (userError "success handler failed")) (either throwE pure)
  failedSuccessRequest <- Client.runClientAsync failedSuccess (retryRequest 0)
  expectThrown "success handler failed" (Client.awaitClient failedSuccessRequest)
  expectThrown "success handler failed" (Client.awaitClient failedSuccessRequest)

  rawExecutions <- liftIO (newIORef (0 :: Int))
  finishExecutions <- liftIO (newIORef (0 :: Int))
  let counted = Client.noRetry (pure . Left)
        (\value -> modifyIORef' rawExecutions (+ 1) >> pure (Right value))
        (\outcome -> do
          liftIO (modifyIORef' finishExecutions (+ 1))
          either throwE pure outcome)
  countedRequest <- Client.runClientAsync counted (retryRequest 0)
  countedFirst <- Client.awaitClient countedRequest
  countedSecond <- Client.awaitClient countedRequest
  rawCount <- liftIO (readIORef rawExecutions)
  finishCount <- liftIO (readIORef finishExecutions)
  liftIO $ expect (countedFirst > 0 && countedFirst == countedSecond && rawCount == 1 && finishCount == 2)
    "raw result was repeated or the finalizer did not run per await"

  let throwingPolicy = Client.noRetry (pure . Left)
        (\_ -> ioError (userError "raw handler failed"))
        (either throwE pure)
  failedHandler <- Client.runClientAsync throwingPolicy (retryRequest 0)
  expectThrown "raw handler failed" (Client.awaitClient failedHandler)
  expectThrown "raw handler failed" (Client.awaitClient failedHandler)
  let wrongResult = Client.clientWithEnv clientEnv (Proxy @WrongResultAPI)
  expectInvalid200 (Client.runClient noRetry (wrongResult 0))
#ifndef VANILLA
  let malformedJson = Client.clientWithEnv clientEnv (Proxy @MalformedJsonAPI)
  expectInvalid200 (Client.runClient noRetry malformedJson)
  let noContentRequest = Client.clientWithEnv clientEnv (Proxy @NoContentFailAPI)
  expectStatus503 (Client.runClient noRetry (noContentRequest 1))
  let immediateRequest :: Client.ClientRequest Int
      immediateRequest onSuccess _ = onSuccess (Response (Just 200) mempty Nothing 1)
      immediatePolicy = Client.noRetry (pure . Left)
        (\_ -> ioError (userError "synchronous handler failed")) (either throwE pure)
  immediate <- Client.runClientAsync immediatePolicy immediateRequest
  expectThrown "synchronous handler failed" (Client.awaitClient immediate)
  let failedLaunch :: Client.ClientRequest Int
      failedLaunch _ _ = ioError (userError "request launch failed")
      launchPolicy = Client.RetryPolicy
        { Client.retryInitialState = 0 :: Int
        , Client.retryOnError = \attempt err -> case err of
            Client.RequestException _ | attempt == 0 -> pure (Left 1)
            Client.RequestException _ -> pure (Right (attempt + 1))
            _ -> fail "request launch exception was not retryable"
        , Client.retryOnSuccess = \_ -> pure (-1)
        , Client.retryFinish = pure
        } :: Client.RetryPolicy (ExceptT Client.ClientError IO) Int Int
  launchAttempts <- Client.runClient launchPolicy failedLaunch
  liftIO $ expect (launchAttempts == 2) "request launch was not retried"
#endif
  let unavailable = Client.clientWithEnv
        (Client.mkClientEnv manager (Client.mkBaseUrl Client.Http "127.0.0.1" (testPort + 1) ""))
        (Proxy @TestAPI)
      _ :<|> unavailableRetry = unavailable
      connectionPolicy = Client.noRetry (pure . Left) (pure . Right) (either throwE pure)
  connectionResult <- runExceptT (Client.runClient connectionPolicy (unavailableRetry 0))
  liftIO $ case connectionResult of
#ifdef VANILLA
    Left (Client.RequestException _) -> pure ()
    _ -> failTest "connection error was not retryable ClientError"
#else
    Left (Client.InvalidResponse Nothing message) ->
      expect (not (Text.null message) && message /= "Request failed") "missing fetch rejection reason"
    _ -> failTest "fetch rejection did not preserve its missing status"
#endif
  liftIO $ consoleLog "SUCCESS"

expectStatus503 :: ExceptT Client.ClientError IO a -> ExceptT Client.ClientError IO ()
expectStatus503 request =
  (void request >> liftIO (failTest "expected HTTP 503")) `catchE` \err ->
    liftIO $ case err of
      Client.HttpError 503 message -> expect (not (Text.null message)) "missing HTTP error message"
      _ -> failTest "unexpected retry failure"

expectInvalid200 :: ExceptT Client.ClientError IO a -> ExceptT Client.ClientError IO ()
expectInvalid200 request =
  (void request >> liftIO (failTest "expected malformed 200 response")) `catchE` \err ->
    liftIO $ case err of
      Client.InvalidResponse (Just 200) message -> expect (not (Text.null message)) "missing decode error"
      _ -> failTest "malformed 200 response was not InvalidResponse"

expectThrown :: Text -> ExceptT Client.ClientError IO a -> ExceptT Client.ClientError IO ()
expectThrown message action = do
  outcome <- Catch.try (void action) :: ExceptT Client.ClientError IO (Either SomeException ())
  liftIO $ case outcome of
    Left exception -> expect (unpack message `isInfixOf` show exception) "unexpected policy exception"
    Right () -> failTest "policy exception did not reach await"

expect :: Bool -> Text -> IO ()
expect condition message = unless condition (failTest message)

failTest :: Text -> IO a
failTest message = consoleError ("ERROR: " <> message) >> Exit.exitFailure