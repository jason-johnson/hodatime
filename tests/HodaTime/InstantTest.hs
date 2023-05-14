module HodaTime.InstantTest
(
  instantTests
)
where

import Test.Tasty
import Test.Tasty.HUnit

import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch)
import Data.HodaTime.TimeZone (utc)
import Data.HodaTime.ZonedDateTime (fromInstant, toLocalTime, ZonedDateTime)
import Data.HodaTime.Calendar.Gregorian (Gregorian)
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

-- TODO: Make sure round-tripping Instant->ZoneDateTime->Instant ends up with the same number.  If it doesn't, it's a problem with leap seconds

test_fromSecondsSinceUnixEpoch :: Assertion  -- TODO: this is temporary until we get the required infrastructure in place to properly test Instants (we can't now because there is no legal conversion, only the internal fromInstant function)
test_fromSecondsSinceUnixEpoch = do
  posT <- getPOSIXTime
  tz <- utc
  let
    secs = round posT
    zdt :: ZonedDateTime Gregorian
    zdt = flip fromInstant tz . fromSecondsSinceUnixEpoch $ secs
    lt = toLocalTime zdt
    utcT = posixSecondsToUTCTime posT
    (LocalTime _ tod) = utcToLocalTime (hoursToTimeZone 0) utcT
    todT = (todHour tod, todMin tod, round . todSec $ tod)
    t = (get hour lt, get minute lt, get second lt)
    str = "time(" ++ show secs ++ "): "
  assertEqual str todT t