🍜 servant-miso-client
===================================

This is a [servant-client](https://github.com/haskell-servant/servant) binding to [miso](https://github.com/dmjio/miso).

### Retry policies with the compatibility client

`Servant.Client.Compat` applies a `RetryPolicy m a result` to an endpoint's
`ClientRequest a`. The policy runs attempts and retry decisions in `IO` and
stores a caller-chosen raw outcome. `retryFinish` converts that outcome in the
caller's monad; the raw type is internal to the policy. For example, an endpoint
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
  , retryOnException = \_ -> pure (-1)
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
to `MaybeT IO a`. Unexpected IO exceptions from requests or raw handlers are
passed to `retryOnException` in `m`, without being thrown to the caller by the
retry loop. IO decisions and outcome handlers run once; repeated awaits reuse
the raw outcome but run `retryFinish` (or `retryOnException`) again. Native
calls start one `async` worker for all attempts; browser calls fill one result
MVar only when the IO retry loop finishes.


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
