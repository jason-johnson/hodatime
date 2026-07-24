module HodaTime.Calendar.IslamicTest
(
  islamicTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Maybe (fromJust, isJust)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import HodaTime.Util
import Data.HodaTime.CalendarDate (day, monthl, month, year, next, previous, dayOfWeek, DayNth(..), CalendarDate)
import Data.HodaTime.Calendar.Islamic (calendarDate, calendarDate', fromNthDay, fromWeekDate, IslamicBcl, IslamicBase15, Month(..), DayOfWeek(..))
import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch)
import Data.HodaTime.TimeZone (utc)
import Data.HodaTime.ZonedDateTime (fromInstant, ZonedDateTime)
import qualified Data.HodaTime.ZonedDateTime as Z

islamicTests :: TestTree
islamicTests = testGroup "Islamic Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [roundTripProps, lensProps, nthDayProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [structureUnits, leapPatternUnits, crossCalendarUnits]

-- | Decode an Islamic date to (day, 1-based month, year) for explicit expected-value assertions.
ymd :: CalendarDate IslamicBcl -> (Int, Int, Int)
ymd x = (get day x, succ . fromEnum $ month x, get year x)

-- | Data.Time has no Islamic calendar, so we verify the construct -> decode bijection directly.
roundTripProps :: TestTree
roundTripProps = testGroup "Constructor"
  [
    QC.testProperty "construct -> decode round-trips" testRoundTrip
  ]
  where
    testRoundTrip (RandomIslamicDate y m d) = (ymd <$> calendarDate d m y) == Just (d, succ (fromEnum m), y)

lensProps :: TestTree
lensProps = testGroup "Lens"
  [
     QC.testProperty "dayOfWeek . next n dow $ date == dow" testNextDoW
    ,QC.testProperty "next n (dayOfWeek date) date == modify (+ n * 7) day date" $ testDirection next (+)
    ,QC.testProperty "previous n (dayOfWeek date) date == modify (- n * 7) day date" $ testDirection previous $ flip (-)
  ]
  where
    epochDay = fromJust $ calendarDate 1 Muharram 1443
    testNextDoW dow (Positive n) = (dayOfWeek . next n dow $ epochDay) == dow
    testDirection dir adjust (Positive n) = dir n (dayOfWeek epochDay) epochDay == modify (adjust $ n * 7) day epochDay

-- | 'fromNthDay' and 'fromWeekDate' are the generic constructors instantiated for Islamic.  Every Islamic month has at
--   least 29 days, so a given weekday always occurs and the properties are total.
nthDayProps :: TestTree
nthDayProps = testGroup "fromNthDay / fromWeekDate"
  [
     QC.testProperty "fromNthDay First dow is the first such weekday (day 1..7)" testFirst
    ,QC.testProperty "fromNthDay Last dow is the last such weekday (final week)" testLast
    ,QC.testProperty "fromWeekDate lands on the requested day-of-week" testWeekDoW
  ]
  where
    testFirst dow (RandomIslamicDate y m _) = let r = fromNthDay First dow m y in (dayOfWeek <$> r) == Just dow && maybe False (\d -> get day d >= 1 && get day d <= 7) r
    testLast dow (RandomIslamicDate y m _) = let r = fromNthDay Last dow m y in (dayOfWeek <$> r) == Just dow && maybe False (\d -> get day d >= 22) r
    testWeekDoW dow (RandomIslamicDate y _ _) = maybe True ((== dow) . dayOfWeek) (fromWeekDate 1 dow y)

structureUnits :: TestTree
structureUnits = testGroup "Structure"
  [
     testCase "30 Muharram is valid (odd months have 30 days)" $ (ymd <$> calendarDate 30 Muharram 1443) @?= Just (30, 1, 1443)
    ,testCase "30 Safar is invalid (even months have 29 days)" $ calendarDate 30 Safar 1443 @?= Nothing
    ,testCase "29 Safar is valid" $ (ymd <$> calendarDate 29 Safar 1443) @?= Just (29, 2, 1443)
    ,testCase "30 Ramadan is valid (Ramadan has 30 days)" $ (ymd <$> calendarDate 30 Ramadan 1443) @?= Just (30, 9, 1443)
    ,testCase "30 DhulHijjah is valid in the leap year 1442" $ (ymd <$> calendarDate 30 DhulHijjah 1442) @?= Just (30, 12, 1442)
    ,testCase "30 DhulHijjah is invalid in the non-leap year 1443" $ calendarDate 30 DhulHijjah 1443 @?= Nothing
    ,testCase "29 DhulHijjah is valid in a non-leap year (1443)" $ (ymd <$> calendarDate 29 DhulHijjah 1443) @?= Just (29, 12, 1443)
    ,testCase "1 Muharram + 1 month == 1 Safar" $ (ymd <$> (modify (+1) monthl <$> calendarDate 1 Muharram 1443)) @?= Just (1, 2, 1443)
    ,testCase "1 DhulQadah + 1 month == 1 DhulHijjah (11th -> 12th month)" $ (ymd <$> (modify (+1) monthl <$> calendarDate 1 DhulQadah 1443)) @?= Just (1, 12, 1443)
    ,testCase "year 0 is out of range" $ calendarDate 1 Muharram 0 @?= Nothing
  ]

-- | The leap pattern is carried in the type, so the same year is a leap year under one pattern and not another, and the
--   two calendars are distinct types.  Base16 (the 'IslamicBcl' default) makes cycle-year 16 a leap year, while Base15
--   makes cycle-year 15 one instead.
leapPatternUnits :: TestTree
leapPatternUnits = testGroup "Leap-pattern selection"
  [
     testCase "cycle-year 16 is leap in Base16 (default): 30 DhulHijjah valid"  $ isJust (calendarDate 30 DhulHijjah 16) @?= True
    ,testCase "cycle-year 16 is not leap in Base15: 30 DhulHijjah invalid"       $ (calendarDate' 30 DhulHijjah 16 :: Maybe (CalendarDate IslamicBase15)) @?= Nothing
    ,testCase "cycle-year 15 is leap in Base15: 30 DhulHijjah valid"             $ isJust (calendarDate' 30 DhulHijjah 15 :: Maybe (CalendarDate IslamicBase15)) @?= True
    ,testCase "cycle-year 15 is not leap in Base16 (default): 30 DhulHijjah invalid" $ calendarDate 30 DhulHijjah 15 @?= Nothing
  ]

-- | The strongest checks: the same absolute day, anchored via Data.Time, must decode to the expected Islamic date.  The
--   epoch (Islamic 1.Muharram.1 = 18.Jul.622 CE proleptic Gregorian, the astronomical epoch) and a few well-known dates
--   of the tabular calendar are pinned via the Gregorian calendar.
crossCalendarUnits :: TestTree
crossCalendarUnits = testGroup "Cross-calendar (Instant)"
  [
     testCase "epoch: 18.Jul.622 (Gregorian) is Islamic 1 Muharram 1"     $ islamicOfDay (fromGregorian 622 7 18) >>= (@?= (1, 1, 1))
    ,testCase "1 Muharram 1443 = 9.Aug.2021 (tabular)"                    $ islamicOfDay (fromGregorian 2021 8 9) >>= (@?= (1443, 1, 1))
    ,testCase "1 Ramadan 1443 = 2.Apr.2022 (tabular)"                     $ islamicOfDay (fromGregorian 2022 4 2) >>= (@?= (1443, 9, 1))
  ]

-- | View the UTC midnight of a Data.Time 'Day' as an Islamic date, returning (year, 1-based month, day).
islamicOfDay :: Day -> IO (Int, Int, Int)
islamicOfDay dt = do
  tz <- utc
  let secs = round (utcTimeToPOSIXSeconds (UTCTime dt 0))
      zdt = fromInstant (fromSecondsSinceUnixEpoch secs) tz :: ZonedDateTime IslamicBcl
  return (Z.year zdt, succ (fromEnum (Z.month zdt)), Z.day zdt)
