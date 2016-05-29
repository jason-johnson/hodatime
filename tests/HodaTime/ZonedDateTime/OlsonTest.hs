module HodaTime.ZonedDateTime.OlsonTest
(
  olsonTests
)
where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.HodaTime.ZonedDateTime (getTransitions)
import Data.Time.LocalTime.TimeZone.Olson (getTimeZoneSeriesFromOlsonFile)
import qualified Data.ByteString.Lazy as L

olsonTests :: TestTree
olsonTests = testGroup "Olson Tests" [scProps, qcProps, unitTests]

scProps = testGroup "(checked by SmallCheck)"
  [
    SC.testProperty "sort == sort . reverse" $ \list -> (list :: [Int]) == reverse list
  ]

qcProps = testGroup "(checked by SmallCheck)"
  [
  ]

unitTests = testGroup "Unit tests"
  [
    testCase "test_getTransitions" $ test_getTransitions
  ]

test_getTransitions :: Assertion  -- TODO: this is temporary until we get the required infrastructure in place to properly test Instants (we can't now because there is no legal conversion, only the internal fromInstant function)
test_getTransitions = do
  let path = "/usr/share/zoneinfo/Europe/Rome"
  transitions <- getTrans' path
  assertFailure "yea right"
  where
    getTrans' path = (either (const []) id . getTransitions) <$> L.readFile path
