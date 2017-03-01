module Network.Pushover where

import Data.Text (Text)
import qualified Data.Text as T

data Request =
  Request { token :: APIToken
          , userKey :: Text}
