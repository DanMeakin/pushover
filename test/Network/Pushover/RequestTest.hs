{-# LANGUAGE OverloadedStrings #-}
module Network.Pushover.RequestTest where

import Data.ByteString.Char8 (ByteString)
import Network.Pushover.Request
import Network.Pushover.Token
import Test.Tasty
import Test.Tasty.HUnit

unitTests = testGroup "Unit tests"
  [ testCase "Encode request" $
      requestQueryPairs testRequest @?= expectedQueryPairs
  ]

testRequest = Request
  { token             = makeTokenOrError "KzGDORePKggMaC0QOYAMyEEuzJnyUi"
  , userKey           = makeTokenOrError "e9e1495ec75826de5983cd1abc8031"
  , message           = "Backup of database \"example\" finished in 16 minutes."
  , devices           = ["droid4"]
  , title             = Just "Backup finished - SQL1"
  , url               = Nothing
  , priority          = Nothing
  , timestamp         = Nothing
  , notificationSound = Nothing
  }

expectedQueryPairs :: [(ByteString, Maybe ByteString)]
expectedQueryPairs = 
  [ ("token",   Just "KzGDORePKggMaC0QOYAMyEEuzJnyUi")
  , ("user",    Just "e9e1495ec75826de5983cd1abc8031")
  , ("message", Just "Backup of database \"example\" finished in 16 minutes.")
  , ("device",  Just "droid4")
  , ("title",   Just "Backup finished - SQL1")
  ]
