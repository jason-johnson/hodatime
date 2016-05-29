module HodaTime.InstantTest
(
  instantTests
)
where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch, fromInstant)
import Data.HodaTime.LocalTime (hour, minute, second)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.LocalTime (todHour, todMin, todSec, hoursToTimeZone, utcToLocalTime, LocalTime(..))

instantTests :: TestTree
instantTests = testGroup "Instant Tests" [scProps, qcProps, unitTests]

scProps = testGroup "(checked by SmallCheck)"
  [
    SC.testProperty "sort == sort . reverse" $ \list -> (list :: [Int]) == reverse list
  ]

qcProps = testGroup "(checked by SmallCheck)"
  [
  ]

unitTests = testGroup "Unit tests"
  [
     testCase "test_fromSecondsSinceUnixEpoch" test_fromSecondsSinceUnixEpoch
  ]

test_fromSecondsSinceUnixEpoch :: Assertion  -- TODO: this is temporary until we get the required infrastructure in place to properly test Instants (we can't now because there is no legal conversion, only the internal fromInstant function)
test_fromSecondsSinceUnixEpoch = do
  posT <- getPOSIXTime
  let secs = round posT
      lt = fromInstant . fromSecondsSinceUnixEpoch $ secs
      utc = posixSecondsToUTCTime posT
      (LocalTime _ tod) = utcToLocalTime (hoursToTimeZone 0) utc
  assertEqual "hours: " (hour lt) (todHour tod)
  assertEqual "minutes: " (minute lt) (todMin tod)
  assertEqual "seconds: " (second lt) (round . todSec $ tod)
