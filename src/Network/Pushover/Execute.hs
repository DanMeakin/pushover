{-# LANGUAGE OverloadedStrings #-}
module Network.Pushover.Execute where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Typeable
import Network.Pushover.Request (Request, defaultRequest, requestQueryPairs, Message)
import Network.Pushover.Response (Response)
import Network.Pushover.Reader (PushoverReader (..))
import Network.Pushover.Token

import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (newTlsManager)

endpoint = "https://api.pushover.net/1/messages.json"

createRequest :: (MonadReader r m, PushoverReader r) => Message -> m Request
createRequest message = do
  apiTk <- asks apiToken
  usrK  <- asks userKey
  return $ defaultRequest apiTk usrK message  

sendRequest :: ( MonadIO m
               , MonadThrow m
               , MonadReader r m
               , PushoverReader r
               ) 
            => Message 
            -> m Response
sendRequest message = do
  manager <- liftIO newTlsManager
  initialRequest <- Http.parseRequest endpoint
  pushoverRequest <- createRequest message
  let request =
        Http.setQueryString (requestQueryPairs pushoverRequest)
          $ initialRequest
              { Http.method = "POST" }
  response <- liftIO $ Http.httpLbs request manager
  case decode $ Http.responseBody response of
       Just resp ->
         return resp

       Nothing ->
         throwM ResponseDecodeException

data PushoverException
  = ResponseDecodeException
  deriving (Show, Typeable)

instance Exception PushoverException
