import Network.Pushover.RequestTest

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Tests" [unitTests]
