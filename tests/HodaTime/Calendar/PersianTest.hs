module HodaTime.Calendar.PersianTest
(
  persianTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Maybe (fromJust)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import HodaTime.Util
import Data.HodaTime.CalendarDate (day, monthl, month, year, next, previous, dayOfWeek, DayNth(..), CalendarDate)
import Data.HodaTime.Calendar.Persian (calendarDate, fromNthDay, fromWeekDate, Persian, Month(..), DayOfWeek(..))
import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch)
import Data.HodaTime.TimeZone (utc)
import Data.HodaTime.ZonedDateTime (fromInstant, ZonedDateTime)
import qualified Data.HodaTime.ZonedDateTime as Z

persianTests :: TestTree
persianTests = testGroup "Persian Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [roundTripProps, lensProps, nthDayProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [structureUnits, crossCalendarUnits]

-- | Decode a Persian date to (day, 1-based month, year) for explicit expected-value assertions.
ymd :: CalendarDate Persian -> (Int, Int, Int)
ymd x = (get day x, succ . fromEnum $ month x, get year x)

-- | Data.Time has no Persian calendar, so we verify the construct -> decode bijection directly.
roundTripProps :: TestTree
roundTripProps = testGroup "Constructor"
  [
    QC.testProperty "construct -> decode round-trips" testRoundTrip
  ]
  where
    testRoundTrip (RandomPersianDate y m d) = (ymd <$> calendarDate d m y) == Just (d, succ (fromEnum m), y)

lensProps :: TestTree
lensProps = testGroup "Lens"
  [
     QC.testProperty "dayOfWeek . next n dow $ date == dow" testNextDoW
    ,QC.testProperty "next n (dayOfWeek date) date == modify (+ n * 7) day date" $ testDirection next (+)
    ,QC.testProperty "previous n (dayOfWeek date) date == modify (- n * 7) day date" $ testDirection previous $ flip (-)
  ]
  where
    epochDay = fromJust $ calendarDate 1 Farvardin 1400
    testNextDoW dow (Positive n) = (dayOfWeek . next n dow $ epochDay) == dow
    testDirection dir adjust (Positive n) = dir n (dayOfWeek epochDay) epochDay == modify (adjust $ n * 7) day epochDay

-- | 'fromNthDay' and 'fromWeekDate' are the generic constructors instantiated for Persian.  Every Persian month has at
--   least 29 days, so a given weekday always occurs and the properties are total.
nthDayProps :: TestTree
nthDayProps = testGroup "fromNthDay / fromWeekDate"
  [
     QC.testProperty "fromNthDay First dow is the first such weekday (day 1..7)" testFirst
    ,QC.testProperty "fromNthDay Last dow is the last such weekday (final week)" testLast
    ,QC.testProperty "fromWeekDate lands on the requested day-of-week" testWeekDoW
  ]
  where
    testFirst dow (RandomPersianDate y m _) = let r = fromNthDay First dow m y in (dayOfWeek <$> r) == Just dow && maybe False (\d -> get day d >= 1 && get day d <= 7) r
    testLast dow (RandomPersianDate y m _) = let r = fromNthDay Last dow m y in (dayOfWeek <$> r) == Just dow && maybe False (\d -> get day d >= 22) r
    testWeekDoW dow (RandomPersianDate y _ _) = maybe True ((== dow) . dayOfWeek) (fromWeekDate 1 dow y)

structureUnits :: TestTree
structureUnits = testGroup "Structure"
  [
     testCase "31 Farvardin is valid (months 1-6 have 31 days)" $ (ymd <$> calendarDate 31 Farvardin 1400) @?= Just (31, 1, 1400)
    ,testCase "31 Mehr is invalid (months 7-11 have 30 days)" $ calendarDate 31 Mehr 1400 @?= Nothing
    ,testCase "30 Mehr is valid" $ (ymd <$> calendarDate 30 Mehr 1400) @?= Just (30, 7, 1400)
    ,testCase "30 Esfand is valid in the leap year 1399" $ (ymd <$> calendarDate 30 Esfand 1399) @?= Just (30, 12, 1399)
    ,testCase "30 Esfand is invalid in the non-leap year 1400" $ calendarDate 30 Esfand 1400 @?= Nothing
    ,testCase "30 Esfand is valid in the leap year 1403" $ (ymd <$> calendarDate 30 Esfand 1403) @?= Just (30, 12, 1403)
    ,testCase "30 Esfand is invalid in 1407 (the 1403->1408 five-year gap)" $ calendarDate 30 Esfand 1407 @?= Nothing
    ,testCase "29 Esfand is valid in a non-leap year (1400)" $ (ymd <$> calendarDate 29 Esfand 1400) @?= Just (29, 12, 1400)
    ,testCase "1 Farvardin + 1 month == 1 Ordibehesht" $ (ymd <$> (modify (+1) monthl <$> calendarDate 1 Farvardin 1400)) @?= Just (1, 2, 1400)
    ,testCase "1 Bahman + 1 month == 1 Esfand (11th -> 12th month)" $ (ymd <$> (modify (+1) monthl <$> calendarDate 1 Bahman 1400)) @?= Just (1, 12, 1400)
    ,testCase "year 0 is out of range" $ calendarDate 1 Farvardin 0 @?= Nothing
    ,testCase "year 1600 is out of range" $ calendarDate 1 Farvardin 1600 @?= Nothing
  ]

-- | The strongest checks: the same absolute day, anchored via Data.Time, must decode to the expected Persian date.  The
--   epoch (Persian 1.Farvardin.1 = 22.Mar.622 CE proleptic Gregorian) and several well-known Nowruz (Persian new year)
--   dates are pinned via the Gregorian calendar.  1404 is a year where the astronomical calendar differs from the
--   arithmetic one (arithmetic Nowruz 1404 = 20.Mar.2025; astronomical = 21.Mar.2025).
crossCalendarUnits :: TestTree
crossCalendarUnits = testGroup "Cross-calendar (Instant)"
  [
     testCase "epoch: 22.Mar.622 (Gregorian) is Persian 1 Farvardin 1" $ persianOfDay (fromGregorian 622 3 22) >>= (@?= (1, 1, 1))
    ,testCase "Nowruz 1400 = 21.Mar.2021 (Gregorian)"                   $ persianOfDay (fromGregorian 2021 3 21) >>= (@?= (1400, 1, 1))
    ,testCase "Nowruz 1399 = 20.Mar.2020 (Gregorian, leap year)"        $ persianOfDay (fromGregorian 2020 3 20) >>= (@?= (1399, 1, 1))
    ,testCase "Nowruz 1404 = 21.Mar.2025 (astronomical, differs from arithmetic)" $ persianOfDay (fromGregorian 2025 3 21) >>= (@?= (1404, 1, 1))
    ,testCase "1.Dey.1348 = 22.Dec.1969 (Gregorian)"                    $ persianOfDay (fromGregorian 1969 12 22) >>= (@?= (1348, 10, 1))
  ]

-- | View the UTC midnight of a Data.Time 'Day' as a Persian date, returning (year, 1-based month, day).
persianOfDay :: Day -> IO (Int, Int, Int)
persianOfDay dt = do
  tz <- utc
  let secs = round (utcTimeToPOSIXSeconds (UTCTime dt 0))
      zdt = fromInstant (fromSecondsSinceUnixEpoch secs) tz :: ZonedDateTime Persian
  return (Z.year zdt, succ (fromEnum (Z.month zdt)), Z.day zdt)
