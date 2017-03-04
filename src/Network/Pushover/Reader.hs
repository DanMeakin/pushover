{-| This module exposes types intended to make it easy to incorporate Pushover
request functionality within an existing application.

The 'PushoverReader' class is designed to make it easy to extend an existing
reader environment to include the information required by Pushover in the making
of requests.

'PushoverKeys' is a simple type available for immediate use should a developer
not yet have a reader environment and wants quickly to use Pushover.
-}
module Network.Pushover.Reader where

import Control.Monad.Error.Class
import Control.Monad.Except
import Data.Text (Text)
import Network.Pushover.Exceptions
import Network.Pushover.Token

-- | The 'PushoverReader' class is intended to make it straightforward to
--   incorporate the making of Pushover requests within an existing monad
--   stack.
--
-- This class is intended to make it easy to add an API token and user key
-- to an existing reader monad environment.
class PushoverReader r where
  apiToken :: r -> APIToken
  userKey  :: r -> UserKey

-- | A basic type for use in storing the pair of keys required for making
--   a request.
data PushoverKeys = PushoverKeys
  { _apiToken :: APIToken
  , _userKey  :: UserKey
  } deriving Show

instance PushoverReader PushoverKeys where
  apiToken =
    _apiToken
  
  userKey =
    _userKey

-- | An unvalidated API token.
type UnvalidatedAPIToken 
  = Text

-- | An unvalidated user key.
type UnvalidatedUserKey
  = Text

-- | Construct a 'PushoverKeys' value.
--
-- This attempts to create valid tokens/keys from a pair of unvalidated
-- tokens/keys, returning the result wrapped within a MonadError.
createKeys :: (Error e, MonadError e m)
           => UnvalidatedAPIToken 
           -> UnvalidatedUserKey 
           -> m PushoverKeys
createKeys apiTkn usrKey =
  PushoverKeys
    <$> makeTokenM apiTkn
    <*> makeTokenM usrKey
