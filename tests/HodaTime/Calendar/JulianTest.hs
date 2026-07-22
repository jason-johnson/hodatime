module HodaTime.Calendar.JulianTest
(
  julianTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Maybe (fromJust)
import Data.Time.Calendar.Julian (fromJulianValid, toJulian)

import HodaTime.Util
import Data.HodaTime.CalendarDate (day, monthl, month, year, next, previous, dayOfWeek, DayNth(..), CalendarDate)
import Data.HodaTime.Calendar.Julian (calendarDate, fromNthDay, fromWeekDate, Julian, Month(..), DayOfWeek(..))

julianTests :: TestTree
julianTests = testGroup "Julian Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [constructorProps, lensProps, nthDayProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [constructorUnits, lensUnits]

-- | Decode a Julian date to (day, 1-based month, year) for explicit expected-value assertions.
ymd :: CalendarDate Julian -> (Int, Int, Int)
ymd x = (get day x, succ . fromEnum $ month x, get year x)

-- | Differential test: 'Data.Time.Calendar.Julian' is the proleptic-Julian oracle.  This is the same shape as the
--   Gregorian constructor property, but it exercises Julian's simpler every-4-years leap rule (so e.g. 1900 is a
--   leap year here, unlike in the Gregorian calendar).
constructorProps :: TestTree
constructorProps = testGroup "Constructor"
  [
    QC.testProperty "same dates as Data.Time" $ testConstructor
  ]
    where
      areSame Nothing Nothing = True
      areSame (Just hdate) (Just date) =
        let
          (ty, tm, tday) = toJulian date
        in get day hdate == tday && (convertMonth . month $ hdate) == tm && get year hdate == fromIntegral ty
      areSame _ _ = False
      convertMonth = succ . fromEnum
      testConstructor (Positive y) m (Positive d) = areSame (calendarDate d m y') (fromJulianValid (fromIntegral y') (convertMonth m) d)
        where
          y' = 1 + (y `mod` 2400)     -- NOTE: spans early-medieval (pre-1582) through modern years

lensProps :: TestTree
lensProps = testGroup "Lens"
  [
     QC.testProperty "dayOfWeek . next n dow $ date == dow" $ testNextDoW
    ,QC.testProperty "next n (dayOfWeek date) date == modify (+ n * 7) day date" $ testDirection next (+)
    ,QC.testProperty "previous n (dayOfWeek date) date == modify (- n * 7) day date" $ testDirection previous $ flip (-)
    ,QC.testProperty "construct -> decode round-trips" $ testRoundTrip
  ]
  where
    epochDay = fromJust $ calendarDate 1 March 2000
    testNextDoW dow (Positive n) = (dayOfWeek . next n dow $ epochDay) == dow
    testDirection dir adjust (Positive n) = dir n (dayOfWeek epochDay) epochDay == modify (adjust $ n * 7) day epochDay
    testRoundTrip (RandomJulianDate y m d) = (ymd <$> calendarDate d m y) == Just (d, succ (fromEnum m), y)

-- | 'fromNthDay' and 'fromWeekDate' are the generic (calendar-agnostic) constructors instantiated for Julian.  These
--   self-validating properties confirm the shared core works with Julian's own weekday math (no per-calendar formula).
nthDayProps :: TestTree
nthDayProps = testGroup "fromNthDay / fromWeekDate"
  [
     QC.testProperty "fromNthDay First dow is the first such weekday (day 1..7)" testFirst
    ,QC.testProperty "fromNthDay Last dow is the last such weekday (final week)" testLast
    ,QC.testProperty "fromWeekDate lands on the requested day-of-week" testWeekDoW
  ]
  where
    testFirst dow (RandomJulianDate y m _) =
      let r = fromNthDay First dow m y
      in (dayOfWeek <$> r) == Just dow && maybe False (\d -> get day d >= 1 && get day d <= 7) r
    testLast dow (RandomJulianDate y m _) =
      let r = fromNthDay Last dow m y
      in (dayOfWeek <$> r) == Just dow && maybe False (\d -> get day d >= 22) r
    testWeekDoW dow (RandomJulianDate y _ _) =
      maybe True ((== dow) . dayOfWeek) (fromWeekDate 1 dow y)

constructorUnits :: TestTree
constructorUnits = testGroup "Constructor"
  [
     testCase "30 February 2000 is not a valid date" $ calendarDate 30 February 2000 @?= Nothing
    ,testCase "29 February 1900 IS valid in Julian (1900 is a Julian leap year)" $ (ymd <$> calendarDate 29 February 1900) @?= Just (29, 2, 1900)
    ,testCase "29 February 1900 matches Data.Time" $ (ymd <$> calendarDate 29 February 1900) @?= (juYmd <$> fromJulianValid 1900 2 29)
    ,testCase "14 October 1066 is valid (long before the 1582 Gregorian cutoff)" $ (ymd <$> calendarDate 14 October 1066) @?= Just (14, 10, 1066)
    ,testCase "14 October 1066 matches Data.Time" $ (ymd <$> calendarDate 14 October 1066) @?= (juYmd <$> fromJulianValid 1066 10 14)
    ,testCase "fromNthDay Last, when the month ends on that weekday, is the last day (not a week early)" $
       let lastDay = fromJust $ calendarDate 28 February 301    -- Feb 301 (not a Julian leap year) ends on a Friday
       in fromNthDay Last (dayOfWeek lastDay) February 301 @?= Just lastDay
  ]
    where
      juYmd d = let (ty, tm, td) = toJulian d in (td, tm, fromIntegral ty)

lensUnits :: TestTree
lensUnits = testGroup "Lens"
  [
     testCase "31 January 2000 + 1M == 29 February 2000 (2000 is a Julian leap year)" $ (ymd <$> (modify (+1) monthl <$> calendarDate 31 January 2000)) @?= Just (29, 2, 2000)
    ,testCase "31 December 2000 + 1D == 1 January 2001" $ (ymd <$> (modify (+1) day <$> calendarDate 31 December 2000)) @?= Just (1, 1, 2001)
    ,testCase "29 February 1900 + 1Y clamps to 28 February 1901" $ (ymd <$> (modify (+1) year <$> calendarDate 29 February 1900)) @?= Just (28, 2, 1901)
  ]
