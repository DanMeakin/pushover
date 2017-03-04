# Pushover

This is a small library for interacting with the Pushover API from within
Haskell.

Pushover exposes a straightforward API for sending notifications to users of
the Android and iOS Pushover app. Details of the API can be found at 
https://pushover.net/api.

## Usage

This library exposes a number of types which represent a `Request` and a
`Response`, and fields contained within these values. It provides functions
which can be used to create and make requests.

### Basic

A basic request can be made as follows:-

```
>>> apiKey  = makeTokenOrError "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
>>> userKey = makeTokenOrError "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyy"
>>> sendMessage apiK usrK $ text "This is a test"
```

Assuming the keys are correct, this should immediately send the notification
to the user with `userKey`.

The `makeTokenOrError` function shown here should not generally be used. 
Instead, the `makeToken` function checks the validity of the token and wraps
the output in Either.

### Reader-based

A more useful approach is to integrate Pushover into your existing Monad stack.
This can be accomplished using the `createKeys` and `sendMessageM` functions:-

```
>>> keys = createKeys "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyy"
>>> keys >>= runReaderT (sendMessageM $ text "This is a test")
```

The Monad-based functions (all with a trailing `M` in their name) require that
a request is executed within a stack including an instance of MonadError and
MonadIO. `sendMessageM` additionally requires a MonadReader and an instance of
`PushoverReader`. See 
[the Reader module](https://github.com/DanMeakin/pushover/blob/master/src/Network/Pushover/Reader.hs)
for more details.
