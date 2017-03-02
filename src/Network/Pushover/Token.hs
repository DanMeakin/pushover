{-# LANGUAGE OverloadedStrings #-}
module Network.Pushover.Token where

import Control.Monad (unless)
import Data.Aeson
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

-- | Define a type to represent the different types of token or key Pushover
--   requires.
--
-- Pushover requires API token and user keys to be send with requests. This is
-- intended to represent these tokens. It is intended that the 'makeToken'
-- function is used to construct validated tokens.
newtype PushoverToken = PushoverToken
  { getToken :: Text
  } deriving Show

instance ToJSON PushoverToken where
  toJSON =
    toJSON . getToken

instance FromJSON PushoverToken where
  parseJSON (String tkn) =
    case makeToken tkn of
         Right tok ->
           return tok

         Left err ->
           fail err

  -- | API token for making a Pushover request.
type APIToken 
  = PushoverToken

-- | User key for the user receiving a notification.
type UserKey
  = PushoverToken

-- | Error raised in attempting to construct a Pushover token.
type TokenError 
  = String 

-- | Construct a 'PushoverToken' value.
--
-- A 'PushoverToken' consists of exactly 30 alphanumeric characters (both
-- uppercase and lowercase). The input key text is validated to ensure it is
-- the correct length and contains valid characters. 
--
-- A descriptive error is returned where validation fails.
makeToken :: Text -> Either TokenError PushoverToken
makeToken tokenText = do
  unless validateLength $
    Left "user key must be exactly 30 characters long"
  unless validateChars $
    Left "user key must contain only alphanumeric characters"
  return $ PushoverToken tokenText

  where 
        validateLength =
          T.length tokenText == 30

        validateChars =
          T.all isAlphaNum tokenText

-- | Construct a 'PushoverToken' value.
--
-- This is a version of 'makeToken' in which an invalid token will raise an
-- error. It should generally not be used, with 'makeToken' the preferred means
-- to create a token.
makeTokenOrError :: Text -> PushoverToken
makeTokenOrError tokenText =
  case makeToken tokenText of
       Right tkn ->
         tkn

       Left err ->
         error err
