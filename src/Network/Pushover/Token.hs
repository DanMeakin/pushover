{-# LANGUAGE OverloadedStrings #-}
{-| This module contains functionality and types concerning tokens used to
authenticate and direct communications with the Pushover API.

The API requires that an API token be sent with every request for 
authentication purposes, and a user key be sent with every request for the
purpose of identifying the recipient of the message. Both types of token/key
are of the same format, and the 'makeToken' functions work for constructing
both types of token/key.
-}
module Network.Pushover.Token 
  ( -- * Token type
    PushoverToken
    -- ** Aliases
  , APIToken
  , UserKey
    -- * Constructing a token
  , makeToken
  , makeTokenM
  , makeTokenOrError
    -- * Encoding for use in HTTP request
  , encodeToken
  ) where

import Control.Arrow (left)
import Control.Monad (unless)
import Control.Monad.Error.Class
import Control.Monad.Except
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Pushover.Exceptions

-- | Define a type to represent the different types of token or key Pushover
--   requires.
--
-- Pushover requires API token and user keys to be send with requests. This is
-- intended to represent these tokens. It is intended that the 'makeToken'
-- function is used to construct validated tokens.
newtype PushoverToken = PushoverToken
  { getToken :: Text
  } deriving (Show, Eq)

instance ToJSON PushoverToken where
  toJSON =
    toJSON . getToken

instance FromJSON PushoverToken where
  parseJSON (String tkn) =
    case makeToken tkn of
         Right tok ->
           return tok

         Left err ->
           fail $ errorMessage err

  -- | API token for making a Pushover request.
type APIToken 
  = PushoverToken

-- | User key for the user receiving a notification.
type UserKey
  = PushoverToken

-- | Construct a 'PushoverToken' value.
--
-- A 'PushoverToken' consists of exactly 30 alphanumeric characters (both
-- uppercase and lowercase). The input key text is validated to ensure it is
-- the correct length and contains valid characters. 
--
-- A descriptive error is returned where validation fails.
makeToken :: Text -> Either PushoverException PushoverToken
makeToken =
  makeTokenM

-- | Construct a 'PushoverToken' value.
--
-- This is similar to the 'makeToken' function, except that it is generalised
-- over the 'MonadError' monad.
makeTokenM :: (Error e, MonadError e m) 
           => Text 
           -> m PushoverToken
makeTokenM tokenText = do
  unless validateLength $
    throwError . strMsg $ errorMessage InvalidTokenLengthException
  unless validateChars $
    throwError . strMsg $ errorMessage InvalidTokenCharactersException
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
         error $ errorMessage err

-- | Encode a 'PushoverToken' into a bytestring for sending within an HTTP
--   request.
encodeToken :: PushoverToken -> ByteString
encodeToken =
  T.encodeUtf8 . getToken
