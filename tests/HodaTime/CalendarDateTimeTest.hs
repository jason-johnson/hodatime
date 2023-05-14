module HodaTime.CalendarDateTimeTest
(
  calendarDateTimeTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Maybe (fromJust)

import HodaTime.Util
import Data.HodaTime.LocalTime (localTime, HasLocalTime(..))
import Data.HodaTime.CalendarDate (day, monthl, next, previous, dayOfWeek)
import Data.HodaTime.Calendar.Gregorian (calendarDate, Month(..))
import Data.HodaTime.CalendarDateTime (on, at)

calendarDateTimeTests :: TestTree
calendarDateTimeTests = testGroup "CalendarDateTimeTests Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [timeLensProps, dateLensProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [rolloverUnits]

timeLensProps :: TestTree
timeLensProps = testGroup "Time Lens"
  [
     QC.testProperty "get seconds offset" $ testGet second _1
    ,QC.testProperty "get minutes offset" $ testGet minute _2
    ,QC.testProperty "get hours offset" $ testGet hour _3
    ,QC.testProperty "modify seconds offset" $ testF (modify . (+)) second _1 (+) 5
    ,QC.testProperty "modify minutes offset" $ testF (modify . (+)) minute _2 (+) 5
    ,QC.testProperty "modify hours offset" $ testF (modify . (+)) hour _3 (+) 5
    ,QC.testProperty "set seconds offset" $ testF set second _1 const 5
    ,QC.testProperty "set minutes offset" $ testF set minute _2 const 5
    ,QC.testProperty "set hours offset" $ testF set hour _3 const 5
  ]
  where
    mkTime h m s = fromJust $ on <$> localTime h m s 0 <*> calendarDate 1 April 2001   -- We are already controlling that only valid values will be passed in
    offsetEq (s, m, h) off = get second off == s && get minute off == m && get hour off == h
    _1 f (a,b,c) = (\a' -> (a',b,c)) <$> f a
    _2 f (a,b,c) = (\b' -> (a,b',c)) <$> f b
    _3 f (a,b,c) = (\c' -> (a,b,c')) <$> f c
    testGet l l' (RandomTime h m s) = get l (mkTime h m s) == get l' (s, m, h)
    testF f l l' g n (RandomTime h m s) = h < 23 - n && s < 60 - n && m < 60 - n QC.==> offsetEq (modify (g n) l' (s,m,h)) $ f n l (mkTime h m s)

dateLensProps :: TestTree
dateLensProps = testGroup "Date Lens"
  [
     QC.testProperty "first day not changed by month math" $ testMonthAdd 1
    ,QC.testProperty "mid day not changed by month math" $ testMonthAdd 15
    ,QC.testProperty "dayOfWeek . next n dow $ date == dow" $ testNextDoW
    ,QC.testProperty "next n (dayOfWeek date) date == modify (+ n * 7) day date" $ testDirection next (+)
    ,QC.testProperty "previous n (dayOfWeek date) date == modify (- n * 7) day date" $ testDirection previous $ flip (-)
  ]
    where
      mkcd d m y = fromJust $ on <$> localTime 10 10 10 0 <*> calendarDate d m y
      testMonthAdd d (CycleYear y) m add = get day (modify (+ add) monthl $ mkcd d m (y + 1900)) == d  -- NOTE: We fix the year so we don't run out of tests
      testNextDoW dow (Positive n) = (dayOfWeek . next n dow $ epochDay) == dow
      testDirection dir adjust (Positive n) = dir n (dayOfWeek epochDay) epochDay == modify (adjust $ n * 7) day epochDay
      epochDay = mkcd 1 March 2000

rolloverUnits :: TestTree
rolloverUnits = testGroup "Rollover"
  [
     testCase "30.Jan.2000 22:57:57 + 2s == 30.Jan.2000 22:57:59" $ modify (+2) second <$> dt @?= mkLT 22 57 59 0
    ,testCase "30.Jan.2000 22:57:57 + 5s == 30.Jan.2000 22:58:02" $ modify (+5) second <$> dt @?= mkLT 22 58 2 0
    ,testCase "30.Jan.2000 22:57:57 + 5m == 30.Jan.2000 23:02:57" $ modify (+5) minute <$> dt @?= mkLT 23 02 57 0
    ,testCase "30.Jan.2000 22:57:57 + 3h == 31.Jan.2000 01:57:57" $ modify (+3) hour <$> dt @?= mkLTWithRolledDate 1 57 57 0
    ,testCase "30.Jan.2000 22:57:57 + 3723s == 31.Jan.2000 00:00:00" $ modify (+3723) second <$> dt @?= mkLTWithRolledDate 0 0 0 0
    ,testCase "30.Jan.2000 22:57:57 + 3725s == 31.Jan.2000 00:00:02" $ modify (+3725) second <$> dt @?= mkLTWithRolledDate 0 0 2 0
    ,testCase "30.Jan.2000 22:57:57 + 48h == 1.Feb.2000 22:57:57" $ modify (+48) hour <$> dt @?= mkLTWithDate monthRoll 22 57 57 0
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