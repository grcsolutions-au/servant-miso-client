🍜 servant-miso-client
===================================

This is a [servant-client](https://github.com/haskell-servant/servant) binding to [miso](https://github.com/dmjio/miso).

### Running compatibility requests

`runClient` executes one `ClientRequest` and returns its result in `IO`:

```haskell
runClient :: ClientRequest a -> IO (Either ClientError a)
```

The browser implementation starts the Miso request and waits for its success or
error callback. Exceptions raised while processing a callback are rethrown to
the caller. Callers can manage concurrency with `async`'s `withAsync` and
`wait`:

```haskell
import Control.Concurrent.Async (wait, withAsync)

withAsync (runClient request) wait
```

Both native and browser clients report these `ClientError` constructors:

- `HttpError Int Text` for a non-successful HTTP status and its diagnostic.
- `RequestException SomeException` for native transport failures normalized by
  servant-client.
- `InvalidResponse (Maybe Int) Text` for decoding failures or browser fetch
  failures without an HTTP status. A successful HTTP response that cannot be
  decoded retains its status (for example, `Just 200`).

`clientErrorStatus` returns the HTTP status when present. Retry behavior, when
needed, can be implemented by calling `runClient` again according to the
application's own policy.

### Integration tests

```bash
nix develop -c scripts/run-tests
```

The runner builds native, WASM, and GHCJS clients in separate Cabal build
directories and executes all three against a fresh native test server per run.
It also executes WASM and GHCJS in headless Chromium using Playwright. The Nix
development shell provides Node.js, Playwright, Chromium, and the browser WASI
shim; no npm install step is required. The local Miso checkout used by
`cabal.project` and the cross-compilers are also provided by the Nix shell.
The test runner uses Miso's echo server and Playwright launcher directly via
the Nix-provided Bun runtime.


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
