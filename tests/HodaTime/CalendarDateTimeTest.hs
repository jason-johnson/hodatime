module HodaTime.CalendarDateTimeTest
(
  calendarDateTimeTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Maybe (fromJust)

import HodaTime.Util (RandomTime(..), CycleYear(..))
import Data.HodaTime.LocalTime (localTime, hour, minute, second)
import Data.HodaTime.CalendarDate (day, next, previous, dayOfWeek)
import Data.HodaTime.Calendar.Gregorian (calendarDate, Month(..))
import Data.HodaTime.CalendarDateTime (on)
import Data.HodaTime.Period (applyPeriod, days, months, hours, minutes, seconds)

calendarDateTimeTests :: TestTree
calendarDateTimeTests = testGroup "CalendarDateTimeTests Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [timeAccessorProps, datePeriodProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [rolloverUnits]

timeAccessorProps :: TestTree
timeAccessorProps = testGroup "Time accessors"
  [
     QC.testProperty "reads constructed components" $ \(RandomTime h m s) ->
       let value = mkTime h m s in (hour value, minute value, second value) == (h, m, s)
  ]
  where
    mkTime h m s = fromJust $ on <$> localTime h m s 0 <*> calendarDate 1 April 2001

datePeriodProps :: TestTree
datePeriodProps = testGroup "Date periods"
  [
     QC.testProperty "first day not changed by month math" $ testMonthAdd 1
    ,QC.testProperty "mid day not changed by month math" $ testMonthAdd 15
    ,QC.testProperty "dayOfWeek . next n dow $ date == dow" $ testNextDoW
    ,QC.testProperty "next n (dayOfWeek date) date == a positive day period" $ testDirection next id
    ,QC.testProperty "previous n (dayOfWeek date) date == a negative day period" $ testDirection previous negate
  ]
    where
      mkcd d m y = fromJust $ on <$> localTime 10 10 10 0 <*> calendarDate d m y
      testMonthAdd d (CycleYear y) m add = day (applyPeriod (months add) $ mkcd d m (y + 1900)) == d
      testNextDoW dow (Positive n) = (dayOfWeek . next n dow $ epochDay) == dow
      testDirection dir adjust (Positive n) = dir n (dayOfWeek epochDay) epochDay == applyPeriod (days (adjust (n * 7))) epochDay
      epochDay = mkcd 1 March 2000

rolloverUnits :: TestTree
rolloverUnits = testGroup "Rollover"
  [
    testCase "30.Jan.2000 22:57:57 + 2s == 30.Jan.2000 22:57:59" $ applyPeriod (seconds 2) <$> dt @?= mkLT 22 57 59 0
      ,testCase "30.Jan.2000 22:57:57 + 5s == 30.Jan.2000 22:58:02" $ applyPeriod (seconds 5) <$> dt @?= mkLT 22 58 2 0
      ,testCase "30.Jan.2000 22:57:57 + 5m == 30.Jan.2000 23:02:57" $ applyPeriod (minutes 5) <$> dt @?= mkLT 23 02 57 0
      ,testCase "30.Jan.2000 22:57:57 + 3h == 31.Jan.2000 01:57:57" $ applyPeriod (hours 3) <$> dt @?= mkLTWithRolledDate 1 57 57 0
      ,testCase "30.Jan.2000 22:57:57 + 3723s == 31.Jan.2000 00:00:00" $ applyPeriod (seconds 3723) <$> dt @?= mkLTWithRolledDate 0 0 0 0
      ,testCase "30.Jan.2000 22:57:57 + 3725s == 31.Jan.2000 00:00:02" $ applyPeriod (seconds 3725) <$> dt @?= mkLTWithRolledDate 0 0 2 0
      ,testCase "30.Jan.2000 22:57:57 + 48h == 1.Feb.2000 22:57:57" $ applyPeriod (hours 48) <$> dt @?= mkLTWithDate monthRoll 22 57 57 0
  ]
  where
    time = localTime 22 57 57 0
    date = calendarDate 30 January 2000
    rollDate = calendarDate 31 January 2000
    monthRoll = calendarDate 1 February 2000
    dt = on <$> time <*> date
    mkLTWithDate date' h m s n = on <$> localTime h m s n <*> date'
    mkLT = mkLTWithDate date
    mkLTWithRolledDate = mkLTWithDate rollDate