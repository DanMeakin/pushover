{-# LANGUAGE OverloadedStrings #-}
{-| This library provides functionality for interacting with the Pushover API
(@https://www.pushover.net@) from within a Haskell codebase.

Pushover exposes a straightforward API for sending notifications to users of
the Android and iOS Pushover app. Details of the API can be found at
@https://pushover.net/api@.

Requests are defined by the 'Request' type. The 'defaultRequest' function is
provided to allow for the easy creation of simple requests. Each request
requires an API token, user token and a message to be passed. All other
fields are optional and can be set as required.

-}
module Network.Pushover
  ( -- * Sending a notification
    -- ** Regular functions
    sendMessage
  , sendRequest
  , createRequest
    -- ** Monadic functions
  , sendMessageM
  , sendRequestM
  , createRequestM
    -- * Requests
    -- ** Constructing a request
  , Request
  , defaultRequest
    -- ** Constructing a request's message
  , Message
  , message
  , bold
  , italic
  , underline
  , color
  , link
  , text
    -- ** Authenticating a request
  , APIToken
  , UserKey
  , PushoverToken
  , makeToken
    -- ** Other request fields
  , URL (..)
  , Priority (..)
  , NotificationSound (..)
    -- * Response
  , Response (..)
    -- * Reader
  , PushoverReader (..)
  , PushoverKeys (..)
  , createKeys
    -- * Exceptions
  , PushoverException
  , errorMessage
  ) where

import           Network.Pushover.Exceptions
import           Network.Pushover.Execute
import           Network.Pushover.Message
import           Network.Pushover.Reader
import           Network.Pushover.Request
import           Network.Pushover.Response
import           Network.Pushover.Token
