🍜 servant-miso-client
===================================

This is a [servant-client](https://github.com/haskell-servant/servant) binding to [miso](https://github.com/dmjio/miso).

### Retry policies with the compatibility client

`Servant.Client.Compat` applies a `RetryPolicy m a b result` to an endpoint's
`ClientRequest a`. The policy owns the retry state and chooses an `m b` action
for both success and terminal failure. It also specifies how async completion
turns that action into a cached `result`. For example, an endpoint returning
`Int` can use `ExceptT ClientError IO` for terminal actions:

```haskell
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Servant.Client.Compat

policy :: RetryPolicy (ExceptT ClientError IO) Int Int (Either String (Either ClientError Int))
policy = RetryPolicy (0 :: Int) onError pure onException
  (fmap Right . runExceptT) (pure . Left . show)
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
  case response of
    Right (Right value) -> liftIO (print value)
    Right (Left err) -> throwE err
    Left message -> liftIO (putStrLn message)
```

An executable's `main :: IO ()` can interpret `runExceptT (runClientTest request)`
once and handle any remaining `ClientError` there. `runClient policy request`
also composes directly in `ExceptT` for calls that do not need an async handle.

The policy's `onException` handler chooses a terminal action for unexpected
request or retry-decision exceptions; the final argument provides a fallback
result when running a terminal action throws, and must return normally.
The caller chooses both the result shape and whether to propagate a cached
`ClientError` (as the example does explicitly with `throwE`). Retry decisions
(including IO logging or delay) run during the request; the terminal action
runs once before the async result completes. Repeated awaits read the same
result without sending another request or rerunning the terminal action.
Native calls start one `async` worker for all attempts; browser calls fill one
result MVar only when the policy finishes.


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
