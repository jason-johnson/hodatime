module HodaTime.InstantTest
(
  instantTests
)
where

import Test.Tasty
import Test.Tasty.HUnit

import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch, fromInstant)
import Data.HodaTime.LocalTime (HasLocalTime(..))
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.LocalTime (todHour, todMin, todSec, hoursToTimeZone, utcToLocalTime, LocalTime(..))
import HodaTime.Util (get)

instantTests :: TestTree
instantTests = testGroup "Instant Tests" [unitTests]

unitTests :: TestTree
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
      todT = (todHour tod, todMin tod, round . todSec $ tod)
      t = (get hour lt, get minute lt, get second lt)
      str = "time(" ++ show secs ++ "): "
  assertEqual str todT t