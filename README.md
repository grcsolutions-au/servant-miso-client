🍜 servant-miso-client
===================================

This is a [servant-client](https://github.com/haskell-servant/servant) binding to [miso](https://github.com/dmjio/miso).

### Retry policies with the compatibility client

`Servant.Client.Compat` applies a `RetryPolicy m a b` to an endpoint's
`ClientRequest a`. The policy owns the retry state and chooses an `m b` action
for both success and terminal failure. For example, an endpoint returning
`Int` can expose its result in `ExceptT ClientError IO`:

```haskell
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Servant.Client.Compat

policy :: RetryPolicy (ExceptT ClientError IO) Int Int
policy = RetryPolicy (0 :: Int) onError pure onException
  where
    onError retries err = pure $
      if retries < 2 && clientErrorStatus err == Just 503
        then Left (retries + 1)
        else Right (throwE err)
    onException _ = pure (-1)

runClientTest :: ClientRequest Int -> ExceptT ClientError IO ()
runClientTest request = do
  pending <- runClientAsync policy request
  response <- awaitClient pending
  liftIO (print response)
```

An executable's `main :: IO ()` can interpret `runExceptT (runClientTest request)`
once and handle any remaining `ClientError` there. `runClient policy request`
also composes directly in `ExceptT` for calls that do not need an async handle.

The last handler decides how to represent unexpected IO exceptions; it is
separate from the HTTP `ClientError` handler. Retry decisions (including any
IO logging or delay) run during the request. The terminal `m b` action runs
when awaited, so awaiting twice does not send another request but does execute
that action twice. Native calls start one `async` worker for all attempts;
browser calls fill one result MVar only when the policy finishes.


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
