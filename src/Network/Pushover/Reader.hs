module Network.Pushover.Reader where

import Data.Text (Text)
import Network.Pushover.Token

class PushoverReader r where
  apiToken :: r -> APIToken
  userKey  :: r -> UserKey

data PushoverKeys = PushoverKeys
  { _apiToken :: APIToken
  , _userKey  :: UserKey
  }

instance PushoverReader PushoverKeys where
  apiToken =
    _apiToken
  
  userKey =
    _userKey

type UnvalidatedAPIToken 
  = Text

type UnvalidatedUserKey
  = Text

createKeys :: UnvalidatedAPIToken 
           -> UnvalidatedUserKey 
           -> Either TokenError PushoverKeys
createKeys apiTkn usrKey =
  PushoverKeys
    <$> makeToken apiTkn
    <*> makeToken usrKey
