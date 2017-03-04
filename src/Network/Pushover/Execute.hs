{-# LANGUAGE OverloadedStrings #-}
{-| This module exposes a set of functions used for constructing and submitting
requests to the Pushover API.

Each function has two versions: a monadic version and a non-monadic version.
The monadic versions are designed for use within an existing monad transformer
stack within an existing application. The non-monadic versions are designed
for standalone use.

-}
module Network.Pushover.Execute 
  ( -- * Regular functions
    sendMessage
  , sendRequest
  , createRequest
    -- * Monadic functions
  , sendMessageM
  , sendRequestM
  , createRequestM
  , PushoverException (..)
  ) where

import Control.Exception (throw)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (decode)
import Network.Pushover.Message (Message)
import Network.Pushover.Request hiding (userKey)
import Network.Pushover.Response (Response)
import Network.Pushover.Reader (PushoverReader (..))
import Network.Pushover.Exceptions
import Network.Pushover.Token

import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (newTlsManager)

-- | Send a request to the Pushover API.
--
-- Requires a pair of Pushover tokens, together with a 'Message' value. These
-- are used to construct a 'defaultRequest' which is then sent to the API.
sendMessage :: APIToken -> UserKey -> Message -> IO Response
sendMessage apiTk userK =
  sendRequest . defaultRequest apiTk userK

-- | Send a request to the Pushover API.
--
-- This is similar to 'sendMessage', except that a constructed 'Request' must
-- be passed instead of the tokens and message.
sendRequest :: Request -> IO Response
sendRequest =
  sendRequestM

-- | Send a request to the Pushover API.
--
-- This function is designed for use within an existing monad transformer 
-- stack to make it easy to send Pushover notifications from inside existing
-- software.
--
-- The relevant tokens are read from a 'PushoverReader' MonadReader instance.
-- These are then used to construct a 'defaultRequest' containing the passed
-- 'Message' value.
sendMessageM :: ( MonadIO m
                , MonadError e m
                , MonadReader r m
                , PushoverReader r
                ) 
             => Message 
             -> m Response
sendMessageM message = do
  pushoverRequest <- createRequestM message
  liftIO $ sendRequest pushoverRequest


-- | Send a request to the Pushover API.
--
-- This function is designed for use within an existing monad transformer 
-- stack to make it easy to send Pushover notifications from inside existing
-- software.
--
-- This is similar to 'sendMessageM', except that a constructed 'Request' must
-- be passed instead of just the message.
sendRequestM :: ( Error e 
                , MonadError e m
                , MonadIO m
                ) 
             => Request
             -> m Response
sendRequestM pushoverRequest = do
  manager <- newTlsManager
  request <- liftIO $ makeHttpRequest pushoverRequest
  response <- liftIO $ Http.httpLbs request manager
  case decode $ Http.responseBody response of
       Just resp ->
         return resp

       Nothing ->
         throwError . strMsg $ "response decoder error"


-- | Create a standard Pushover request.
--
-- This is an alias of the 'defaultRequest' function.
createRequest :: APIToken -> UserKey -> Message -> Request
createRequest =
  defaultRequest

-- | Create a standard Pushover request.
--
-- This is similar to 'createRequest', except that token information is read
-- from within the monad stack.
createRequestM :: ( MonadReader r m
                  , PushoverReader r
                  )
               => Message 
               -> m Request
createRequestM message = do
  apiTk <- asks apiToken
  usrK  <- asks userKey
  return $ defaultRequest apiTk usrK message  
