🍜 servant-miso-client
===================================

This is a [servant-client](https://github.com/haskell-servant/servant) binding to [miso](https://github.com/dmjio/miso).

### Running compatibility requests

`runClient` executes one `ClientRequest` and returns its result in `IO`:

```haskell
runClient :: ClientRequest a -> IO (Either ClientError a)
```

For scoped async-style usage, `withClientAsync` and `waitClient` provide one
interface on native and browser targets:

```haskell
withClientAsync request $ \pending -> do
  -- perform other work
  waitClient pending
```

Native calls use `async` internally. Browser calls start the Miso request and
fill a result cell from its success/error callback; callback exceptions are
rethrown by `waitClient`. Leaving the browser scope cannot abort an in-flight
fetch.

`ClientError` is opaque. Its normalized fields are available through public
accessors:

- `clientErrorStatus :: ClientError -> Maybe Int`
- `clientErrorMessage :: ClientError -> Text`
- `clientErrorException :: ClientError -> Maybe SomeException`

Native connection failures expose their underlying exception through
`clientErrorException`; browser fetch failures provide a diagnostic message.
Decoding errors retain their response status when available, including `Just
200`. Retry behavior can be implemented by calling `runClient` again according
to the application's own policy.

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
