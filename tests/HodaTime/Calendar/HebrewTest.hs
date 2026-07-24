module HodaTime.Calendar.HebrewTest
(
  hebrewTests
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
import Data.HodaTime.Calendar.Hebrew (calendarDate, calendarDate', fromNthDay, fromWeekDate, HebrewCivil, HebrewScriptural, Month(..), DayOfWeek(..))
import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch)
import Data.HodaTime.TimeZone (utc)
import Data.HodaTime.ZonedDateTime (fromInstant, ZonedDateTime)
import qualified Data.HodaTime.ZonedDateTime as Z

hebrewTests :: TestTree
hebrewTests = testGroup "Hebrew Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [roundTripProps, lensProps, nthDayProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [structureUnits, numberingUnits, crossCalendarUnits]

-- | Decode a civil Hebrew date to (day, 1-based civil month, year) for explicit expected-value assertions.
ymd :: CalendarDate HebrewCivil -> (Int, Int, Int)
ymd x = (get day x, succ . fromEnum $ month x, get year x)

-- | Data.Time has no Hebrew calendar, so we verify the construct -> decode bijection directly.  'RandomHebrewDate'
--   only generates valid dates (the leap month 'AdarI' in leap years, the swing months capped at their shorter length).
roundTripProps :: TestTree
roundTripProps = testGroup "Constructor"
  [
    QC.testProperty "construct -> decode round-trips" testRoundTrip
  ]
  where
    testRoundTrip (RandomHebrewDate y m d) = (ymd <$> calendarDate d m y) == Just (d, succ (fromEnum m), y)

lensProps :: TestTree
lensProps = testGroup "Lens"
  [
     QC.testProperty "dayOfWeek . next n dow $ date == dow" testNextDoW
    ,QC.testProperty "next n (dayOfWeek date) date == modify (+ n * 7) day date" $ testDirection next (+)
    ,QC.testProperty "previous n (dayOfWeek date) date == modify (- n * 7) day date" $ testDirection previous $ flip (-)
  ]
  where
    anchorDay = fromJust $ calendarDate 1 Tishri 5784
    testNextDoW dow (Positive n) = (dayOfWeek . next n dow $ anchorDay) == dow
    testDirection dir adjust (Positive n) = dir n (dayOfWeek anchorDay) anchorDay == modify (adjust $ n * 7) day anchorDay

-- | 'fromNthDay' and 'fromWeekDate' are the generic constructors instantiated for Hebrew.  Every Hebrew month has at
--   least 29 days, so a given weekday always occurs and the properties are total.
nthDayProps :: TestTree
nthDayProps = testGroup "fromNthDay / fromWeekDate"
  [
     QC.testProperty "fromNthDay First dow is the first such weekday (day 1..7)" testFirst
    ,QC.testProperty "fromNthDay Last dow is the last such weekday (final week)" testLast
    ,QC.testProperty "fromWeekDate lands on the requested day-of-week" testWeekDoW
  ]
  where
    testFirst dow (RandomHebrewDate y m _) = let r = fromNthDay First dow m y in (dayOfWeek <$> r) == Just dow && maybe False (\d -> get day d >= 1 && get day d <= 7) r
    testLast dow (RandomHebrewDate y m _) = let r = fromNthDay Last dow m y in (dayOfWeek <$> r) == Just dow && maybe False (\d -> get day d >= 22) r
    testWeekDoW dow (RandomHebrewDate y _ _) = maybe True ((== dow) . dayOfWeek) (fromWeekDate 1 dow y)

-- | The interesting Hebrew structure: the leap month 'AdarI' (present only in leap years) and the two swing months
--   'Cheshvan' \/ 'Kislev' (29 or 30 days depending on the year).  Concrete years: 5784 is leap (deficient); 5783 is a
--   complete common year (Cheshvan 30, Kislev 30); 5786 is a regular common year (Cheshvan 29, Kislev 30); 5781 is a
--   deficient common year (Cheshvan 29, Kislev 29).
structureUnits :: TestTree
structureUnits = testGroup "Structure"
  [
     testCase "30 Tishri 5784 is valid (Tishri always has 30 days)"          $ (ymd <$> calendarDate 30 Tishri 5784) @?= Just (30, 1, 5784)
    ,testCase "29 Adar 5786 is valid (Adar always has 29 days)"              $ (ymd <$> calendarDate 29 Adar 5786) @?= Just (29, 7, 5786)
    ,testCase "30 Cheshvan 5783 is valid (complete year)"                    $ (ymd <$> calendarDate 30 Cheshvan 5783) @?= Just (30, 2, 5783)
    ,testCase "30 Cheshvan 5786 is invalid (regular year: Cheshvan has 29)"  $ calendarDate 30 Cheshvan 5786 @?= Nothing
    ,testCase "30 Kislev 5786 is valid (regular year: Kislev has 30)"        $ (ymd <$> calendarDate 30 Kislev 5786) @?= Just (30, 3, 5786)
    ,testCase "30 Kislev 5781 is invalid (deficient year: Kislev has 29)"    $ calendarDate 30 Kislev 5781 @?= Nothing
    ,testCase "30 AdarI 5784 is valid (leap year has the extra month)"       $ (ymd <$> calendarDate 30 AdarI 5784) @?= Just (30, 6, 5784)
    ,testCase "1 AdarI 5786 is invalid (common year has no AdarI)"           $ calendarDate 1 AdarI 5786 @?= Nothing
    ,testCase "1 Shevat 5784 + 1 month == 1 AdarI (leap year keeps AdarI)"   $ (ymd <$> (modify (+1) monthl <$> calendarDate 1 Shevat 5784)) @?= Just (1, 6, 5784)
    ,testCase "1 Shevat 5784 + 2 months == 1 Adar (leap year)"               $ (ymd <$> (modify (+2) monthl <$> calendarDate 1 Shevat 5784)) @?= Just (1, 7, 5784)
    ,testCase "1 Shevat 5786 + 1 month == 1 Adar (common year skips AdarI)"  $ (ymd <$> (modify (+1) monthl <$> calendarDate 1 Shevat 5786)) @?= Just (1, 7, 5786)
    ,testCase "1 Elul 5785 + 1 month == 1 Tishri 5786 (year rolls at Tishri)"$ (ymd <$> (modify (+1) monthl <$> calendarDate 1 Elul 5785)) @?= Just (1, 1, 5786)
    ,testCase "30 AdarI 5784 + 1 year == 29 Adar 5785 (AdarI -> Adar, common)"$ (ymd <$> (modify (+1) year <$> calendarDate 30 AdarI 5784)) @?= Just (29, 7, 5785)
    ,testCase "year 0 is out of range"                                       $ calendarDate 1 Tishri 0 @?= Nothing
  ]

-- | The numbering is carried in the type: the same physical date is numbered differently by civil ('Tishri' = 1) and
--   scriptural ('Nisan' = 1) conventions, but the underlying day, month value and year are identical.
numberingUnits :: TestTree
numberingUnits = testGroup "Month numbering (civil vs scriptural)"
  [
     testCase "civil: Tishri is month 1"                      $ fromEnum (Tishri :: Month HebrewCivil) @?= 0
    ,testCase "scriptural: Tishri is month 7"                 $ fromEnum (Tishri :: Month HebrewScriptural) @?= 6
    ,testCase "civil: Nisan is month 8"                       $ fromEnum (Nisan :: Month HebrewCivil) @?= 7
    ,testCase "scriptural: Nisan is month 1"                  $ fromEnum (Nisan :: Month HebrewScriptural) @?= 0
    ,testCase "civil: toEnum 0 == Tishri"                     $ (toEnum 0 :: Month HebrewCivil) @?= Tishri
    ,testCase "scriptural: toEnum 0 == Nisan"                 $ (toEnum 0 :: Month HebrewScriptural) @?= Nisan
    ,testCase "same date is Nisan (civil), numbered 8"        $ (succ . fromEnum . month <$> calendarDate 15 Nisan 5784) @?= Just 8
    ,testCase "same date is Nisan (scriptural), numbered 1"   $ (succ . fromEnum . month <$> scripturalNisan) @?= Just 1
    ,testCase "numbering does not move the day/year"          $ (dayYear <$> calendarDate 15 Nisan 5784, dayYear <$> scripturalNisan) @?= (Just (15, 5784), Just (15, 5784))
  ]
  where
    scripturalNisan = calendarDate' 15 Nisan 5784 :: Maybe (CalendarDate HebrewScriptural)
    dayYear x = (get day x, get year x)

-- | The strongest checks: the same absolute day, anchored via Data.Time, must decode to the expected Hebrew date.  The
--   anchors are well-known Gregorian equivalents (Rosh Hashanah of three years and Passover), independently verified.
crossCalendarUnits :: TestTree
crossCalendarUnits = testGroup "Cross-calendar (Instant)"
  [
     testCase "16.Sep.2023 (Gregorian) is 1 Tishri 5784 (Rosh Hashanah)"  $ hebrewOfDay (fromGregorian 2023 9 16) >>= (@?= (5784, 1, 1))
    ,testCase "23.Apr.2024 (Gregorian) is 15 Nisan 5784 (Passover)"       $ hebrewOfDay (fromGregorian 2024 4 23) >>= (@?= (5784, 8, 15))
    ,testCase "3.Oct.2024 (Gregorian) is 1 Tishri 5785 (Rosh Hashanah)"   $ hebrewOfDay (fromGregorian 2024 10 3) >>= (@?= (5785, 1, 1))
    ,testCase "23.Sep.2025 (Gregorian) is 1 Tishri 5786 (Rosh Hashanah)"  $ hebrewOfDay (fromGregorian 2025 9 23) >>= (@?= (5786, 1, 1))
  ]

-- | View the UTC midnight of a Data.Time 'Day' as a civil Hebrew date, returning (year, 1-based civil month, day).
hebrewOfDay :: Day -> IO (Int, Int, Int)
hebrewOfDay dt = do
  tz <- utc
  let secs = round (utcTimeToPOSIXSeconds (UTCTime dt 0))
      zdt = fromInstant (fromSecondsSinceUnixEpoch secs) tz :: ZonedDateTime HebrewCivil
  return (Z.year zdt, succ (fromEnum (Z.month zdt)), Z.day zdt)
