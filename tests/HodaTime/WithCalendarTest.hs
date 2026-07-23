{-# LANGUAGE FlexibleContexts #-}    -- for the polymorphic 'ymd' helper's Enum (MoY d) constraint
{-# LANGUAGE ScopedTypeVariables #-}
module HodaTime.WithCalendarTest
(
  withCalendarTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe (fromJust)

import HodaTime.Util (get)
import Data.HodaTime.CalendarDate (withCalendar, day, month, year, CalendarDate, HasDate, MoY)
import qualified Data.HodaTime.CalendarDateTime as CDT
import qualified Data.HodaTime.ZonedDateTime as Z
import qualified Data.HodaTime.Calendar.Gregorian as G
import qualified Data.HodaTime.Calendar.Julian as J
import qualified Data.HodaTime.Calendar.Coptic as C
import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch)
import Data.HodaTime.TimeZone (utc)

withCalendarTests :: TestTree
withCalendarTests = testGroup "withCalendar Tests" [dateTests, dateTimeTests, zonedTests]

-- | Decode any 'HasDate' value to (day, 1-based month, year).
ymd :: (HasDate d, Enum (MoY d)) => d -> (Int, Int, Int)
ymd x = (get day x, succ . fromEnum $ month x, get year x)

mkG :: Int -> G.Month G.Gregorian -> Int -> CalendarDate G.Gregorian
mkG d m y = fromJust $ G.calendarDate d m y

mkJ :: Int -> J.Month J.Julian -> Int -> CalendarDate J.Julian
mkJ d m y = fromJust $ J.calendarDate d m y

dateTests :: TestTree
dateTests = testGroup "CalendarDate"
  [
     testCase "Gregorian 17.Jun.2020 -> Julian 4.Jun.2020 (13 days behind)" $
       ymd (withCalendar (mkG 17 G.June 2020) :: CalendarDate J.Julian) @?= (4, 6, 2020)
    ,testCase "Gregorian 11.Sep.2021 -> Coptic 1 Thout 1738 (Nayrouz)" $
       ymd (withCalendar (mkG 11 G.September 2021) :: CalendarDate C.Coptic) @?= (1, 1, 1738)
    ,testCase "Julian 29.Aug.284 -> Coptic 1 Thout 1 (Coptic epoch)" $
       ymd (withCalendar (mkJ 29 J.August 284) :: CalendarDate C.Coptic) @?= (1, 1, 1)
    ,testCase "Gregorian -> Julian -> Gregorian round-trips" $
       let g = mkG 17 G.June 2020
       in (withCalendar (withCalendar g :: CalendarDate J.Julian) :: CalendarDate G.Gregorian) @?= g
  ]

dateTimeTests :: TestTree
dateTimeTests = testGroup "CalendarDateTime"
  [
     testCase "Gregorian 17.Jun.2020 (start of day) -> Julian 4.Jun.2020" $
       ymd (CDT.withCalendar (CDT.atStartOfDay (mkG 17 G.June 2020)) :: CDT.CalendarDateTime J.Julian) @?= (4, 6, 2020)
    ,testCase "Gregorian -> Julian -> Gregorian round-trips (keeps the LocalTime)" $
       let cdt = CDT.atStartOfDay (mkG 17 G.June 2020)
       in (CDT.withCalendar (CDT.withCalendar cdt :: CDT.CalendarDateTime J.Julian) :: CDT.CalendarDateTime G.Gregorian) @?= cdt
  ]

zonedTests :: TestTree
zonedTests = testGroup "ZonedDateTime"
  [
     testCase "same Instant (UTC), Gregorian view -> Julian view is 13 days behind" $ do
       tz <- utc
       let inst = fromSecondsSinceUnixEpoch 1592352000     -- 2020-06-17T00:00:00Z
           zdtG = Z.fromInstant inst tz :: Z.ZonedDateTime G.Gregorian
           zdtJ = Z.withCalendar zdtG :: Z.ZonedDateTime J.Julian
       (Z.year zdtJ, succ (fromEnum (Z.month zdtJ)), Z.day zdtJ) @?= (2020, 6, 4)
    ,testCase "round-trips back to the same Instant" $ do
       tz <- utc
       let inst = fromSecondsSinceUnixEpoch 1592352000
           zdtG = Z.fromInstant inst tz :: Z.ZonedDateTime G.Gregorian
           zdtJ = Z.withCalendar zdtG :: Z.ZonedDateTime J.Julian
       Z.toInstant zdtJ @?= inst
  ]
