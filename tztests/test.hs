import Test.Tasty

import HodaTime.ZonedDateTimeTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [zonedDateTimeTests]
