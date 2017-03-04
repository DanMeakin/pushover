{-# LANGUAGE OverloadedStrings #-}
module Network.Pushover.RequestTest where

import Data.ByteString.Char8 (ByteString)
import Network.HTTP.Client hiding (defaultRequest)
import Network.Pushover.Message (text)
import Network.Pushover.Request
import Network.Pushover.Token
import Test.Tasty
import Test.Tasty.HUnit

unitTests = testGroup "Unit tests"
  [ testCase "Default request initialises only passed fields" testDefaultRequest
  , testCase "Correct query string encoded from request" testQueryStrings
  ]

testRequest = Request
  { requestToken      = makeTokenOrError "KzGDORePKggMaC0QOYAMyEEuzJnyUi"
  , requestUserKey    = makeTokenOrError "e9e1495ec75826de5983cd1abc8031"
  , requestMessage    = text "Backup of database \"example\" finished in 16 minutes."
  , devices           = ["droid4"]
  , title             = Just "Backup finished - SQL1"
  , url               = Nothing
  , priority          = Nothing
  , timestamp         = Nothing
  , notificationSound = Nothing
  }

--------------------------------------------------------------------------------

testDefaultRequest =
  let tkn =
        makeTokenOrError "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

      usrK =
        makeTokenOrError "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

      msg =
        text "Msg"
      
  in
      actualDefaultRequest tkn usrK msg @?= expectedDefaultRequest tkn usrK msg

expectedDefaultRequest tkn usrK msg = Request
  { requestToken      = tkn
  , requestUserKey    = usrK
  , requestMessage    = msg
  , devices           = []
  , title             = Nothing
  , url               = Nothing
  , priority          = Nothing
  , timestamp         = Nothing
  , notificationSound = Nothing
  }

actualDefaultRequest =
  defaultRequest

--------------------------------------------------------------------------------

testQueryStrings =
  actualQueryString >>= (@?= expectedQueryString)

actualQueryString =
  fmap queryString . makeHttpRequest $ testRequest

expectedQueryString =
  "?token=KzGDORePKggMaC0QOYAMyEEuzJnyUi&\
  \user=e9e1495ec75826de5983cd1abc8031&\
  \message=Backup%20of%20database%20%22example%22%20finished%20in%2016%20minutes.&\
  \device=droid4&\
  \title=Backup%20finished%20-%20SQL1&\
  \html=1"
