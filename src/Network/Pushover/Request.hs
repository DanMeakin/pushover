{-# LANGUAGE OverloadedStrings #-}
module Network.Pushover.Request 
  ( -- * Constructing a request
    Request (..)
  , defaultRequest
    -- * Constructing a request's message
  , Message
  , makeMessage
    -- * Other request parameters
  , URL (..)
  , Priority (..)
  , NotificationSound (..)
    -- * HTTP request helper
  , makeHttpRequest
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Network.HTTP.Client as Http
import Network.Pushover.Token
import Network.URI.Encode

-- | Pushover API endpoint.
endpoint = "https://api.pushover.net/1/messages.json"

-- | Construct the contents of a Pushover notification request. This follows
--   the API specification at @https://pushover.net/api@.
data Request = Request
  { token     :: APIToken
  -- ^ The API token provided by your Pushover app's dashboard at
  --   @https://pushover.net/apps@.
  , userKey   :: UserKey
  -- ^ The user key of the user receiving this notification, found in the 
  --   Pushover dashboard at @https://pushover.net/dashboard@.
  , message   :: Message
  -- ^ The notification message to push to the user.
  , devices   :: [Text]
  -- ^ An optional list of devices to which to send the notification. If empty,
  --   it will be sent to all of the user's devices.
  , title     :: Maybe Text
  -- ^ An optional title for the message.
  , url       :: Maybe URL
  -- ^ An optional URL for inclusion with the message. 
  , priority  :: Maybe Priority
  -- ^ The priority of this message. This affects way in which the notification 
  --   is presented to the receiving user. See 'Priority' for more information.
  , timestamp :: Maybe UTCTime
  -- ^ An optional timestamp for the notification. If no timestamp is provided,
  --   the time the request is received by the Pushover API is used.
  , notificationSound :: Maybe NotificationSound
  -- ^ The notification sound to use. The default is 'Pushover', with 'None' 
  --   provided for a silent notification.
  } deriving Show

newtype Message = Message
  { getMessage :: Text 
  } deriving Show

-- | A URL for sending within a notification request.
--
-- A Pushover URL is optional within a request; if present, it may optionally
-- contain a title to display instead of the URL itself.
data URL = URL
  { urlPath  :: Text
  , urlTitle :: Maybe Text
  } deriving Show

-- | Describes the priority of a particular message.
--
-- The different priority settings affect the way in which a notification is
-- presented to the user. See @https://pushover.net/api#priority@ for specific
-- details.
data Priority
  = Lowest
  | Low
  | Normal
  | High
  | Emergency
  deriving Show

-- | Describes the notification sound for a notification. 
data NotificationSound 
  = Pushover
  | Bike
  | Bugle
  | CashRegister
  | Classical
  | Cosmic
  | Falling
  | Gamelan
  | Incoming
  | Intermission
  | Magic
  | Mechanical
  | PianoBar
  | Siren
  | SpaceAlarm
  | TugBoat
  | AlienAlarm
  | Climb
  | Persistent
  | Echo
  | UpDown
  | None
  deriving Show

-- | Construct a default request value.
--
-- As a request requires, at a minimum, an API token, a user key and a
-- message, this function requires each of these values as an argument. Other
-- fields can then be initialised using the regular Haskell record syntax.
defaultRequest :: APIToken -> UserKey -> Message -> Request
defaultRequest apiToken usrKey msg =
  Request { token             = apiToken
          , userKey           = usrKey
          , message           = msg
          , devices           = []
          , title             = Nothing
          , url               = Nothing
          , priority          = Nothing
          , timestamp         = Nothing
          , notificationSound = Nothing
          }

-- | Construct a 'Message' value from a 'Text' value.
makeMessage :: Text -> Message
makeMessage =
  Message

-- | Construct an HTTP request out of a Pushover request value.
--
-- This function is exposed for use by the functions in the 
-- "Network.Pushover.Execute" module. It is unlikely that the user will
-- require to call it directly.
makeHttpRequest :: Request -> IO Http.Request
makeHttpRequest pushoverRequest = do
  initialRequest <- Http.parseRequest endpoint
  return . Http.setQueryString (requestQueryPairs pushoverRequest)
             $ initialRequest { Http.method = "POST" }

-- | Create a set of HTTP query pairs from a 'Request'.
requestQueryPairs :: Request -> [(ByteString, Maybe ByteString)]
requestQueryPairs =
  filter present . makePairs

  where
        makePairs :: Request -> [(ByteString, Maybe ByteString)]
        makePairs request =
          (fmap . fmap) ($ request)
            [ ("token"     , encodeValue . getToken . token)
            , ("user"      , encodeValue . getToken . userKey)
            , ("message"   , encodeValue . getMessage . message)
            , ("device"    , encodeValue . T.intercalate "," . devices)
            , ("title"     , encodeMaybe . title )
            , ("url"       , encodeMaybe . reqUrl)
            , ("url_title" , encodeMaybe . reqUrlTitle)
            , ("priority"  , fmap encodePriority . priority)
            , ("timestamp" , fmap encodeTimestamp . timestamp)
            , ("sound"     , fmap encodeSound . notificationSound)
            ]

          where 
                reqUrl req =
                  urlPath <$> url req

                reqUrlTitle req =
                  url req >>= urlTitle

                encodeMaybe =
                  fmap T.encodeUtf8

                encodeValue =
                  Just . T.encodeUtf8

        present (_, Nothing) = False
        present (_, Just _)  = True

-- | Encode a timestamp into a bytestring.
--
-- Used when converting a 'Request' for sending.
encodeTimestamp :: UTCTime -> ByteString
encodeTimestamp =
  B.pack . show . round . utcTimeToPOSIXSeconds

-- | Encode a priority into a bytestring.
--
-- Used when converting a 'Request' for sending.
encodePriority :: Priority -> ByteString
encodePriority Emergency =  "2"
encodePriority High      =  "1"
encodePriority Normal    =  "0"
encodePriority Low       = "-1"
encodePriority Lowest    = "-2"

-- | Encode a notification sound into a bytestring.
--
-- Used when converting a 'Request' for sending.
encodeSound :: NotificationSound -> ByteString
encodeSound Pushover     = "pushover"
encodeSound Bike         = "bike"
encodeSound Bugle        = "bugle"
encodeSound CashRegister = "cashregister"
encodeSound Classical    = "classical"
encodeSound Cosmic       = "cosmic"
encodeSound Falling      = "falling"
encodeSound Gamelan      = "gamelan"
encodeSound Incoming     = "incoming"
encodeSound Intermission = "intermission"
encodeSound Magic        = "magic"
encodeSound Mechanical   = "mechanical"
encodeSound PianoBar     = "pianobar"
encodeSound Siren        = "siren"
encodeSound SpaceAlarm   = "spacealarm"
encodeSound TugBoat      = "tugboat"
encodeSound AlienAlarm   = "alienalarm"
encodeSound Climb        = "climb"
encodeSound Persistent   = "persistent"
encodeSound Echo         = "echo"
encodeSound UpDown       = "updown"
encodeSound None         = "none"
