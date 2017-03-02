{-# LANGUAGE OverloadedStrings #-}
module Network.Pushover.Response where

import Data.Aeson hiding (Success)
import Data.Text (Text)
import qualified Data.Text as T

-- | Describes a response received to a notification request. This follows the
--   specification at @https://pushover.net/api#response@.
--
-- A request will either be successful or it will be unsuccessful. Where it is
-- successful, an appropriate status indicator is returned. Where unsuccessful,
-- the response will contain an errors key with a list of errors within the 
-- request.
--
-- In both cases, a request parameter is returned which uniquely identifies the
-- request leading to the response.
data Response = Response
  { status  :: ResponseStatus
  , request :: Text
  } deriving Show

instance FromJSON Response where
  parseJSON (Object v) =
    Response
      <$> parseJSON (Object v)
      <*> v .: "request"

-- | Describes a Pushover response status.
--
-- A request is either successful or unsuccessful. This is reflected in this
-- type. Success has no additional data associated with it; failure has a list
-- of errors associated.
data ResponseStatus 
  = Success
  | Failure [RequestError]
  deriving Show

instance FromJSON ResponseStatus where
  parseJSON (Object v) = do
    status <- v .: "status"
    case (status :: Integer) of
         1 ->
           pure Success

         0 ->
           Failure <$> v .: "errors"
              
type RequestError =
  Text

