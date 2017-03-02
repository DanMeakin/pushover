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

A basic request can be made as follows:-

```
apiKey  = makeTokenOrError "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
userKey = makeTokenOrError "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyy"

sendMessage apiK usrK $ makeMessage "This is a test"
```

Assuming the keys are correct, this should immediately send the notification
to the user with `userKey`.

The `makeTokenOrError` function shown here should not generally be used. 
Instead, the `makeToken` function checks the validity of the token and wraps
the output in Either.
