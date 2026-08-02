module HodaTime.PeriodTest
(
  periodTests
)
where

import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit

import Data.HodaTime.Calendar.Gregorian
  (Gregorian, Month(..), calendarDate)
import Data.HodaTime.CalendarDate (CalendarDate)
import Data.HodaTime.CalendarDateTime (CalendarDateTime, on)
import qualified Data.HodaTime.Calendar.Hebrew as Hebrew
import Data.HodaTime.LocalTime (LocalTime, localTime)
import Data.HodaTime.Period

periodTests :: TestTree
periodTests = testGroup "Period"
  [ testCase "months clamp at the end of the target month" $
      applyPeriod (months 1) jan31 @?= date 29 February 2000
  , testCase "combined months are applied as one component" $
      applyPeriod (months 1 <> months 1) jan31 @?= date 31 March 2000
  , testCase "fields are applied from largest to smallest" $
      applyPeriod (years 1 <> months 1) leapDay @?= date 28 March 2001
  , testCase "time-only periods wrap LocalTime" $
      applyPeriod (hours 2) lateTime @?= time 1 30 0 0
  , testCase "negative subsecond periods wrap LocalTime" $
      applyPeriod (nanoseconds (-1)) midnightTime @?=
        time 23 59 59 999999999
  , testCase "large time periods do not overflow the representation" $
      applyPeriod (hours 100000) midnightTime @?= time 16 0 0 0
  , testCase "mixed periods carry time overflow into the date" $
      applyPeriod (months 1 <> hours 2) lateDateTime @?=
        on (time 1 30 0 0) (date 1 March 2000)
  , testCase "month periods follow the target calendar" $
      applyPeriod (months 1) hebrewShevat @?= hebrewAdarI
  , testCase "period composition has an identity" $
      (mempty <> samplePeriod, samplePeriod <> mempty) @?=
        (samplePeriod, samplePeriod)
  , testCase "period composition is associative" $
      ((years 1 <> months 2) <> days 3) @?=
        (years 1 <> (months 2 <> days 3) :: Period (CalendarDate Gregorian))
  ]
  where
    jan31 = date 31 January 2000
    leapDay = date 29 February 2000
    lateTime = time 23 30 0 0
    midnightTime = time 0 0 0 0
    lateDateTime = on lateTime jan31
    hebrewShevat = fromJust (Hebrew.calendarDate 1 Hebrew.Shevat 5784)
    hebrewAdarI = fromJust (Hebrew.calendarDate 1 Hebrew.AdarI 5784)
    samplePeriod = years 1 <> months 2 <> days 3 :: Period (CalendarDate Gregorian)

date :: Int -> Month Gregorian -> Int -> CalendarDate Gregorian
date day month year = fromJust (calendarDate day month year)

time :: Int -> Int -> Int -> Int -> LocalTime
time hour minute second nanosecond =
  fromJust (localTime hour minute second nanosecond)

_mixedPeriodType :: Period (CalendarDateTime Gregorian)
_mixedPeriodType = months 5 <> hours 2