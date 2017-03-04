{-# LANGUAGE OverloadedStrings #-}
{-| This module provides functions for creating and encoding messages for use
within Pushover requests.

Pushover messages contain a very limited subset of HTML. Users can insert bold,
italic, underlined, and colored text, and URLs within messages. The 'Message'
type represents all of these possible formatting options.

Constructing a 'Message' is done through the use of the 'message', 'bold',
'italic', 'underline', 'color', 'link', and 'text' functions. The 'message'
function takes a list of parts created using the other functions, and
concatenates these into a single message. Different types of formatting can
be nested within each other by simply calling the functions on the results of
other function calls. For example:-

@
  bold
    [ italic
        [ text "This is bold & italic"
        ]
    , text "This is bold"
    , underline
        [ text "This is bold & underlined"
        ]
    ]
@

will create a message with the text values formatted as described.

-}
module Network.Pushover.Message
  ( -- * Creating a Message
    Message
  , message
  , bold
  , italic
  , underline
  , color
  , link
  , text
    -- * Encoding for inclusion in request
  , encodeMessage
    -- * Attributes
  , ColorCode
  , makeColorCode
  , Url
  ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Numeric               (showHex)

-- | Represents a message sent to the Pushover API.
--
-- A Pushover message can be constructed with a very small subset of HTML.
-- This type represents the available HTML formatting for a message.
data Message
  = Parts           [Message]
  | Bold            [Message]
  | Italic          [Message]
  | Underline       [Message]
  | Color ColorCode [Message]
  | Link Url        [Message]
  | MessageText Text
  deriving (Show, Eq)

-- | Represents an HTML color code.
--
-- A ColorCode consists of a red, a green and a blue element, each of which
-- must have a value of between 0 and 255. This type cannot enforce this
-- constraint, but see 'makeColorCode' which does.
data ColorCode = ColorCode
  { red   :: Integer
  , green :: Integer
  , blue  :: Integer
  } deriving (Show, Eq)

type Url
  = Text

-- | Make a message from a list of message parts.
message :: [Message] -> Message
message =
  Parts

-- | Make a bold message.
bold :: [Message] -> Message
bold =
  Bold

-- | Make an italic message.
italic :: [Message] -> Message
italic =
  Italic

-- | Make an underlined message.
underline :: [Message] -> Message
underline =
  Underline

-- | Make a message with colored text.
--
-- Accepts three integer arguments for red, green and blue color elements,
-- respectively.
color :: Integer -> Integer -> Integer -> [Message] -> Message
color r g b =
  Color (makeColorCode r g b)

-- | Make a url message.
link :: Url -> [Message] -> Message
link =
  Link

-- | Make a textual message.
text :: Text -> Message
text =
  MessageText

-- | Construct a 'ColorCode' value.
--
-- A 'ColorCode' requires a red, a green and a blue value for construction.
-- This function takes these as arguments and returns a constructed
-- 'ColorCode'.
--
-- This function checks that each element is within the required 0-255 range.
-- Any element which is not is rounded to the nearest extrema (0 for negative
-- values; 255 for values larger than that number).
makeColorCode :: Integer -> Integer -> Integer -> ColorCode
makeColorCode r g b =
  ColorCode
    { red   = f r
    , green = f g
    , blue  = f b
    }

  where
        f =
          max 0 . min 255

-- | Encode a 'Message' into a bytestring.
--
-- This function is intended to convert a 'Message' into a form useable within
-- a 'Request'. It generates a bytestring containing the HTML for the message.
encodeMessage :: Message -> ByteString
encodeMessage =
  enc

  where
        enc msg =
          case msg of
               Parts msg ->
                 encInner msg

               Bold msg ->
                 wrapTag "b" "" msg

               Italic msg ->
                 wrapTag "i" "" msg

               Underline msg ->
                 wrapTag "u" "" msg

               Color colorCode msg ->
                 wrapTag "font" (colorAttr colorCode) msg

               Link url msg ->
                 wrapTag "a" (linkAttr url) msg

               MessageText txt ->
                 T.encodeUtf8 txt

        encInner =
          B.concat . map enc

        wrapTag tagName attr inner =
          B.concat [ openTag tagName attr
                   , encInner inner
                   , closeTag tagName
                   ]

        openTag tagName attr =
          B.concat [ "<"
                   , tagName
                   , attr
                   , ">"
                   ]

        closeTag tagName =
          B.concat [ "</"
                   , tagName
                   , ">"
                   ]

        colorAttr =
          B.append " color=" . encodeColorCode

        linkAttr =
          B.append " href=" . T.encodeUtf8

-- | Encode a 'ColorCode' into a bytestring.
--
-- This converts a 'ColorCode' value into its corresponding hexadecimal
-- representation.
encodeColorCode :: ColorCode -> ByteString
encodeColorCode (ColorCode r g b) =
  B.pack $ '#':hexCode

  where
        hexCode =
          padShow r . padShow g . padShow b $ ""

        padShow =
          (\hex rest -> replicate (2 - length (hex "")) '0' ++ hex rest)
            <$> showHex
