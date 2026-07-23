module HodaTime.Calendar.CopticTest
(
  copticTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Maybe (fromJust)
import Data.Time.Calendar (Day, fromGregorian)
import qualified Data.Time.Calendar.Julian as J
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import HodaTime.Util
import Data.HodaTime.CalendarDate (day, monthl, month, year, next, previous, dayOfWeek, DayNth(..), CalendarDate)
import Data.HodaTime.Calendar.Coptic (calendarDate, fromNthDay, fromWeekDate, Coptic, Month(..), DayOfWeek(..))
import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch)
import Data.HodaTime.TimeZone (utc)
import Data.HodaTime.ZonedDateTime (fromInstant, ZonedDateTime)
import qualified Data.HodaTime.ZonedDateTime as Z

copticTests :: TestTree
copticTests = testGroup "Coptic Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [roundTripProps, lensProps, nthDayProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [structureUnits, crossCalendarUnits]

-- | Decode a Coptic date to (day, 1-based month, year) for explicit expected-value assertions.
ymd :: CalendarDate Coptic -> (Int, Int, Int)
ymd x = (get day x, succ . fromEnum $ month x, get year x)

-- | Data.Time has no Coptic calendar, so we verify the construct -> decode bijection directly.
roundTripProps :: TestTree
roundTripProps = testGroup "Constructor"
  [
    QC.testProperty "construct -> decode round-trips" testRoundTrip
  ]
  where
    testRoundTrip (RandomCopticDate y m d) = (ymd <$> calendarDate d m y) == Just (d, succ (fromEnum m), y)

lensProps :: TestTree
lensProps = testGroup "Lens"
  [
     QC.testProperty "dayOfWeek . next n dow $ date == dow" testNextDoW
    ,QC.testProperty "next n (dayOfWeek date) date == modify (+ n * 7) day date" $ testDirection next (+)
    ,QC.testProperty "previous n (dayOfWeek date) date == modify (- n * 7) day date" $ testDirection previous $ flip (-)
  ]
  where
    epochDay = fromJust $ calendarDate 1 Thout 1716
    testNextDoW dow (Positive n) = (dayOfWeek . next n dow $ epochDay) == dow
    testDirection dir adjust (Positive n) = dir n (dayOfWeek epochDay) epochDay == modify (adjust $ n * 7) day epochDay

-- | 'fromNthDay' and 'fromWeekDate' are the generic constructors instantiated for Coptic.  We skip the short thirteenth
--   month (which has fewer than 7 days, so a given weekday may not occur) to keep the properties total.
nthDayProps :: TestTree
nthDayProps = testGroup "fromNthDay / fromWeekDate"
  [
     QC.testProperty "fromNthDay First dow is the first such weekday (day 1..7)" testFirst
    ,QC.testProperty "fromNthDay Last dow is the last such weekday (final week)" testLast
    ,QC.testProperty "fromWeekDate lands on the requested day-of-week" testWeekDoW
  ]
  where
    testFirst dow (RandomCopticDate y m _)
      | m == PiKogiEnavot = True
      | otherwise = let r = fromNthDay First dow m y in (dayOfWeek <$> r) == Just dow && maybe False (\d -> get day d >= 1 && get day d <= 7) r
    testLast dow (RandomCopticDate y m _)
      | m == PiKogiEnavot = True
      | otherwise = let r = fromNthDay Last dow m y in (dayOfWeek <$> r) == Just dow && maybe False (\d -> get day d >= 24) r
    testWeekDoW dow (RandomCopticDate y _ _) = maybe True ((== dow) . dayOfWeek) (fromWeekDate 1 dow y)

structureUnits :: TestTree
structureUnits = testGroup "Structure"
  [
     testCase "30 Thout is valid" $ (ymd <$> calendarDate 30 Thout 1716) @?= Just (30, 1, 1716)
    ,testCase "31 Thout is invalid (months are 30 days)" $ calendarDate 31 Thout 1716 @?= Nothing
    ,testCase "5 PiKogiEnavot is valid in a non-leap year (1732)" $ (ymd <$> calendarDate 5 PiKogiEnavot 1732) @?= Just (5, 13, 1732)
    ,testCase "6 PiKogiEnavot is valid in a leap year (1731, 1731 mod 4 == 3)" $ (ymd <$> calendarDate 6 PiKogiEnavot 1731) @?= Just (6, 13, 1731)
    ,testCase "6 PiKogiEnavot is invalid in a non-leap year (1732)" $ calendarDate 6 PiKogiEnavot 1732 @?= Nothing
    ,testCase "1 Thout + 1 month == 1 Paopi" $ (ymd <$> (modify (+1) monthl <$> calendarDate 1 Thout 1716)) @?= Just (1, 2, 1716)
    ,testCase "1 Mesori + 1 month == 1 PiKogiEnavot (12th -> 13th month)" $ (ymd <$> (modify (+1) monthl <$> calendarDate 1 Mesori 1716)) @?= Just (1, 13, 1716)
  ]

-- | The strongest checks: the same absolute day, anchored via Data.Time, must decode to the expected Coptic date.
--   The epoch (Coptic 1.Thout.1 = Julian 29.Aug.284) is pinned via 'Data.Time.Calendar.Julian', and two well-known
--   Nayrouz (Coptic new year) dates are pinned via the Gregorian calendar.
crossCalendarUnits :: TestTree
crossCalendarUnits = testGroup "Cross-calendar (Instant)"
  [
     testCase "epoch: Julian 29.Aug.284 is Coptic 1 Thout 1"    $ copticOfDay (J.fromJulian 284 8 29) >>= (@?= (1, 1, 1))
    ,testCase "Nayrouz 1738 = 11.Sep.2021 (Gregorian)"          $ copticOfDay (fromGregorian 2021 9 11) >>= (@?= (1738, 1, 1))
    ,testCase "Nayrouz 1716 = 12.Sep.1999 (Gregorian, pre-leap)" $ copticOfDay (fromGregorian 1999 9 12) >>= (@?= (1716, 1, 1))
  ]

-- | View the UTC midnight of a Data.Time 'Day' as a Coptic date, returning (year, 1-based month, day).
copticOfDay :: Day -> IO (Int, Int, Int)
copticOfDay dt = do
  tz <- utc
  let secs = round (utcTimeToPOSIXSeconds (UTCTime dt 0))
      zdt = fromInstant (fromSecondsSinceUnixEpoch secs) tz :: ZonedDateTime Coptic
  return (Z.year zdt, succ (fromEnum (Z.month zdt)), Z.day zdt)
