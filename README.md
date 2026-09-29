🍜 servant-miso-client
===================================

This is a [servant-client](https://github.com/haskell-servant/servant) binding to [miso](https://github.com/dmjio/miso).

### Retry policies with the compatibility client

`Servant.Client.Compat` applies a `RetryPolicy m a result` to an endpoint's
`ClientRequest a`. The policy runs attempts and retry decisions in `IO` and
stores a caller-chosen raw outcome. `retryFinish` converts that outcome in the
caller's monad; the raw type is internal to the policy. Both native and browser
clients report the same `ClientError` constructors:

- `HttpError Int Text` for a non-successful HTTP status and its diagnostic.
- `RequestException SomeException` for a thrown request or transport failure.
- `InvalidResponse (Maybe Int) Text` for decoding failures or browser fetch
  failures without an HTTP status. A successful HTTP response that cannot be
  decoded retains its status (for example, `Just 200`).

`clientErrorStatus` returns the status when present. For example, an endpoint
returning `Int` can use `ExceptT ClientError IO`:

```haskell
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Servant.Client.Compat

policy :: RetryPolicy (ExceptT ClientError IO) Int Int
policy = RetryPolicy
  { retryInitialState = 0 :: Int
  , retryOnError = onError
  , retryOnSuccess = pure . Right
  , retryFinish = either throwE pure
  }
  where
    onError retries err = pure $
      if retries < 2 && clientErrorStatus err == Just 503
        then Left (retries + 1)
        else Right (Left err)

runClientTest :: ClientRequest Int -> ExceptT ClientError IO ()
runClientTest request = do
  pending <- runClientAsync policy request
  response <- awaitClient pending
  liftIO (print response)
```

An executable's `main :: IO ()` can interpret `runExceptT (runClientTest request)`
once and handle any remaining `ClientError` there. `runClient policy request`
returns the same result in `ExceptT` for calls that do not need an async handle.

The policy can use other raw outcomes and monads, such as `Maybe a` converted
to `MaybeT IO a`. Request and transport failures can reach `retryOnError` and
be retried; exceptions from policy hooks are never retried or converted into
`ClientError`. They are cached and rethrown on await via `MonadThrow m`. IO
decisions and outcome handlers run once; repeated awaits reuse the raw outcome
but run `retryFinish` again in `m`. Exceptions from `retryFinish` occur on each
await. Native calls start one `async` worker for all attempts; browser calls
fill one final-result MVar when the IO retry loop finishes.

### Integration tests

```bash
nix develop -c scripts/run-tests
```

The runner builds native, WASM, and GHCJS clients in separate Cabal build
directories and executes all three against a fresh native test server per run.
It also executes WASM and GHCJS in headless Chromium using Playwright, with a
locally installed browser WASI shim (installed via `npm ci` on first run).
The Nix development shell provides Chromium on Linux; set `CHROMIUM_BIN` to a
different Chromium executable when needed. The local Miso checkout used by
`cabal.project`, the cross-compilers, Node.js, and npm are also required.


```haskell
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Miso
import Miso.JSON
import Miso.Html.Element as H
import Miso.Html.Event as H
-----------------------------------------------------------------------------
import Data.Proxy
import Servant.Miso.Client
import Servant.API
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents myComponent
  { mount = Just Start
  }
-----------------------------------------------------------------------------
type MyComponent = App () Action
-----------------------------------------------------------------------------
myComponent :: MyComponent
myComponent = component () update_ $ \() ->
  H.div_ []
  [ button_ [ onClick Download ] [ "download" ]
  ] where
      update_ = \case
        Download -> do
          io_ (consoleLog "clicked")
          downloadGithub Downloaded DownloadError
        DownloadError Response {..} -> io_ $ do
          consoleError $ ms (show errorMessage)
        Downloaded Response {..} -> io_ $ do
          consoleLog $ ms $ show body
        Start -> io_ $ do
          consoleLog "starting..."
-----------------------------------------------------------------------------
data Action
  = Downloaded (Response Value)
  | DownloadError (Response MisoString)
  | Download
  | Start
-----------------------------------------------------------------------------
type GitHubAPI = Get '[JSON] Value
-----------------------------------------------------------------------------
downloadGithub :: (Response Value -> Action) -> (Response MisoString -> Action) -> Effect ROOT () Action
downloadGithub successsful errorful = withSink $ \sink ->
  toClient "https://api.github.com" (Proxy @GitHubAPI) (sink . successsful) (sink . errorful)
-----------------------------------------------------------------------------
```

### Build

```bash
cabal build
```

### Dev

```bash
cabal build
```
