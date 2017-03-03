module Network.Pushover.Exceptions where

import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Except
import Data.Typeable

-- | Defines possible exceptions which can be thrown from execution commands. 
data PushoverException
  = ResponseDecodeException
  -- ^ This exception is thrown when a response is malformed and cannot be
  --   decoded.
  | InvalidTokenCharactersException
  -- ^ This exception is thrown when a token cannot be constructed because it
  --   contains invalid characters.
  | InvalidTokenLengthException
  -- ^ This exception is thrown when a token cannot be constructed because it
  --   is an incorrect length.
  | PushoverException String
  -- ^ This is a generic exception for other errors in Pushover.
  deriving (Show, Typeable)

instance Exception PushoverException

instance Error PushoverException where
  noMsg =
    PushoverException "Unknown exception"

  strMsg =
    PushoverException


-- | Display a description error message for each 'PushoverException' 
--   constructor.
errorMessage :: PushoverException -> String
errorMessage InvalidTokenLengthException =
  "token must be exactly 30 characters long"

errorMessage InvalidTokenCharactersException =
  "token must contain only alphanumeric characters"

errorMessage ResponseDecodeException =
  "unexpected format of Pushover response"
