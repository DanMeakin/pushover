{-# LANGUAGE OverloadedStrings #-}
module Network.Pushover 
  ( -- * Sending a notification
    sendRequest
    -- * Request
  , Pushover.Request
  , Pushover.defaultRequest
  , Pushover.Message
  , Pushover.makeMessage
    -- * Response
  , Pushover.Response
    -- * Tokens
  , APIToken
  , UserKey
  , PushoverToken
  , makeToken
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Network.Pushover.Request as Pushover
import Network.Pushover.Response as Pushover
import Network.Pushover.Token
import Network.HTTP.Client as Http

endpoint = "https://api.pushover.net/1/messages.json"

sendRequest :: Pushover.Request -> Manager -> IO (Either String Pushover.Response)
sendRequest req manager = do
  initialRequest <- Http.parseRequest endpoint
  let request =
        setQueryString (requestQueryPairs req)
          $ initialRequest
              { method = "POST"
              }
  response <- Http.httpLbs request manager
  return . eitherDecode . responseBody $ response

{-|
POST an HTTPS request to https://api.pushover.net/1/messages.json with the following parameters:

    token (required) - your application's API token
    user (required) - the user/group key (not e-mail address) of your user (or you), viewable when logged into our dashboard (often referred to as USER_KEY in our documentation and code examples)
    message (required) - your message 

Some optional parameters may be included:

    device - your user's device name to send the message directly to that device, rather than all of the user's devices (multiple devices may be separated by a comma)
    title - your message's title, otherwise your app's name is used
    url - a supplementary URL to show with your message
    url_title - a title for your supplementary URL, otherwise just the URL is shown
    priority - send as -2 to generate no notification/alert, -1 to always send as a quiet notification, 1 to display as high-priority and bypass the user's quiet hours, or 2 to also require confirmation from the user
    timestamp - a Unix timestamp of your message's date and time to display to the user, rather than the time your message is received by our API
    sound - the name of one of the sounds supported by device clients to override the user's default sound choice 
-}
