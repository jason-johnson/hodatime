module HodaTime.InstantTest
(
  instantTests
)
where

import Test.Tasty
import Test.Tasty.HUnit

import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch)
import Data.HodaTime.TimeZone (utc, timeZone)
import Data.HodaTime.ZonedDateTime (fromInstant, toLocalTime, toInstant, ZonedDateTime, year, month, day)
import Data.HodaTime.Calendar.Gregorian (Gregorian)
import Data.HodaTime.LocalTime (HasLocalTime(..))
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Calendar (toGregorian)
import Data.Time.LocalTime (todHour, todMin, todSec, hoursToTimeZone, utcToLocalTime, LocalTime(..))
import qualified System.Info as SysInfo
import HodaTime.Util (get)

instantTests :: TestTree
instantTests = testGroup "Instant Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
     testCase "test_fromSecondsSinceUnixEpoch" test_fromSecondsSinceUnixEpoch
    ,testCase "test_instantDate" test_instantDate
    ,testCase "test_instantRoundTrip" test_instantRoundTrip
  ]

test_fromSecondsSinceUnixEpoch :: Assertion
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

-- | The date (year, month, day) of an 'Instant' converted through the public 'ZonedDateTime' path (in UTC) must
--   agree with Data.Time.  This exercises the cycle-based 'Instant' -> date decode ('daysToGregorian').
test_instantDate :: Assertion
test_instantDate = do
  posT <- getPOSIXTime
  tz <- utc
  let
    secs = round posT
    zdt :: ZonedDateTime Gregorian
    zdt = flip fromInstant tz . fromSecondsSinceUnixEpoch $ secs
    utcT = posixSecondsToUTCTime posT
    (LocalTime dayGreg _) = utcToLocalTime (hoursToTimeZone 0) utcT
    (ty, tm, td) = toGregorian dayGreg
    expected = (fromIntegral ty, tm, td)
    actual = (year zdt, succ . fromEnum $ month zdt, day zdt)
    str = "date(" ++ show secs ++ "): "
  assertEqual str expected actual

-- | 'Instant' -> 'ZonedDateTime' -> 'Instant' (via 'toInstant') must round-trip exactly.  Tested in UTC and in a
--   zone with a non-zero UTC offset (so a sign error in the offset arithmetic would be caught).  A 'ZonedDateTime'
--   maps to exactly one 'Instant', so this conversion is always unambiguous (the ambiguity is on the reverse,
--   CalendarDateTime -> ZonedDateTime, direction).
test_instantRoundTrip :: Assertion
test_instantRoundTrip = mapM_ check ["UTC", euZone]
  where
    euZone = if SysInfo.os == "mingw32" then "W. Europe Standard Time" else "Europe/Zurich"
    check zoneName = do
      tz <- timeZone zoneName
      let
        inst = fromSecondsSinceUnixEpoch 1700000000
        zdt :: ZonedDateTime Gregorian
        zdt = fromInstant inst tz
      assertEqual ("Instant -> ZonedDateTime -> Instant round-trips in " ++ zoneName) inst (toInstant zdt)