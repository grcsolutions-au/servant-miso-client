{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Servant.API.Compat ((:<|>)(..))
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
import Miso.DSL (jsg, new)
import Miso.FFI (Blob(..))
import Miso.String (MisoString, ms)
#endif

main :: IO ()
main = runClientTest

#ifdef wasm32_HOST_ARCH
foreign export javascript "hs_start" main :: IO ()
#endif

fixtureFileContents :: LBS8.ByteString
fixtureFileContents = LBS8.pack "multipart file contents"

expectedAck :: UploadAck
expectedAck = UploadAck "alpha" "beta" "fixture.txt" (pack (LBS8.unpack fixtureFileContents))

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
retryExcept = Client.RetryPolicy (0 :: Int) handleError pure onException
  where
    handleError retries err =
      pure $ if retries < 2 && Client.clientErrorStatus err == Just 503
        then Left (retries + 1)
        else Right (throwE err)
    onException _ = liftIO (failTest "unexpected request exception")

runClientTest :: IO ()
runClientTest = do
  upload <- expectedUpload
  requestBody <- withBoundary upload
  manager <- Client.newManager
  let baseUrl = Client.mkBaseUrl Client.Http "127.0.0.1" testPort ""
      clientEnv = Client.mkClientEnv manager baseUrl
      uploadRequest :<|> retryRequest = Client.clientWithEnv clientEnv (Proxy @TestAPI)
      noRetry = Client.noRetry throwE pure (\_ -> liftIO (failTest "unexpected request exception"))
  asyncRequest <- Client.runClientAsync noRetry (uploadRequest requestBody)
  result <- runExceptT (Client.awaitClient asyncRequest)
  case result of
    Right response
      | response == expectedAck -> pure ()
      | otherwise ->
          failTest ("unexpected response: " <> pack (show response))
    Left _ ->
      failTest "upload request failed"

  retried <- Client.runClientAsync retryExcept (retryRequest 2)
  first <- runExceptT (Client.awaitClient retried)
  second <- runExceptT (Client.awaitClient retried)
  case (first, second) of
    (Right 3, Right 3) -> pure ()
    _ -> failTest "retry result changed across awaits"
  nextAttempt <- runExceptT (Client.runClient noRetry (retryRequest 2))
  case nextAttempt of
    Right 4 -> pure ()
    _ -> failTest "awaiting repeated the HTTP request"

  exhausted <- runExceptT (Client.runClient retryExcept (retryRequest 20))
  case exhausted of
    Left err -> expect (Client.clientErrorStatus err == Just 503) "unexpected retry failure"
    Right _ -> failTest "retry limit was ignored"
  let diagnostic = Client.RetryPolicy (0 :: Int)
        (\retries err -> pure $ if retries < 1
          then Left (retries + 1)
          else Right (pure (retries + 1, Client.clientErrorStatus err)))
        (\value -> pure (value, Nothing))
        (\_ -> liftIO (failTest "unexpected request exception"))
        :: Client.RetryPolicy (ExceptT Client.ClientError IO) Int (Int, Maybe Int)
  stopped <- runExceptT (Client.runClient diagnostic (retryRequest 21))
  case stopped of
    Right (2, Just 503) -> pure ()
    _ -> failTest "policy did not use retry state and error"

  immediate <- runExceptT (Client.runClient noRetry (retryRequest 22))
  case immediate of
    Left err -> expect (Client.clientErrorStatus err == Just 503) "noRetry failed incorrectly"
    Right _ -> failTest "noRetry unexpectedly retried"
  direct <- runExceptT (Client.runClient noRetry (retryRequest 0))
  case direct of
    Right 1 -> pure ()
    _ -> failTest "noRetry success failed"
  converted <- Client.runClient
    (Client.noRetry (\_ -> pure "failed") (pure . show) (\_ -> failTest "unexpected request exception"))
    (retryRequest 0)
  expect (converted == "2") "IO policy did not change the result type"
  let unexpected = Client.RetryPolicy ()
        (\_ _ -> fail "retry decision failed")
        (pure . show)
        (\_ -> pure "recovered")
        :: Client.RetryPolicy IO Int String
  recovered <- Client.runClient unexpected (retryRequest 23)
  expect (recovered == "recovered") "unexpected IO failure bypassed the policy"
  consoleLog "SUCCESS"

expect :: Bool -> Text -> IO ()
expect condition message = unless condition (failTest message)

failTest :: Text -> IO a
failTest message = consoleError ("ERROR: " <> message) >> Exit.exitFailure