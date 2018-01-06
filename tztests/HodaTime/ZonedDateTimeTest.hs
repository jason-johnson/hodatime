module HodaTime.ZonedDateTimeTest
(
  zonedDateTimeTests
)
where

import Test.Tasty
import Test.Tasty.HUnit

import Data.HodaTime.ZonedDateTime (ZonedDateTime, fromCalendarDateTimeLeniently, toCalendarDate, toLocalTime, inDst, zoneAbbreviation)
import Data.HodaTime.TimeZone (timeZone)
import qualified Data.HodaTime.Calendar.Gregorian as G
import Data.HodaTime.Calendar.Gregorian (Month(..))
import Data.HodaTime.LocalTime (localTime, hour, minute, second, Hour, Minute, Second)
import Data.HodaTime.CalendarDateTime (at, year, month, day, CalendarDate, Year, DayOfMonth)

import System.Process (readCreateProcess, proc, env)
import Data.List (intercalate)
import Control.Applicative (Const(..))

zonedDateTimeTests :: TestTree
zonedDateTimeTests = testGroup "ZonedDateTime Tests" [unitTests, fromCalendarDateTimeLeniently2017Tests, fromCalendarDateTimeLeniently2039Tests]

-- top level tests

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
  ]

fromCalendarDateTimeLeniently2017Tests :: TestTree
fromCalendarDateTimeLeniently2017Tests = testGroup "fromCalendarDateTimeLeniently 2017 tests" $ test_fromCalendarDateTimeLenientlyFor 2017

fromCalendarDateTimeLeniently2039Tests :: TestTree
fromCalendarDateTimeLeniently2039Tests = testGroup "fromCalendarDateTimeLeniently 2039 tests" $ test_fromCalendarDateTimeLenientlyFor 2039

-- test functions

test_fromCalendarDateTimeLenientlyFor :: Year -> [TestTree]
test_fromCalendarDateTimeLenientlyFor y = do
  m <- [January .. December]
  d <- [1..28]
  h <- [0,1,2,3,4,18]
  let datestr = intercalate "-" [show y, show m, show d]
  let timestr = intercalate ":" [show h, "11", "12"]
  let caseStr = datestr ++ "T" ++ timestr
  return . testCase caseStr $ test_getDates y m d h 11 12

test_getDates :: Int -> G.Month G.Gregorian -> Int -> Int -> Int -> Int -> Assertion
test_getDates y m d h mm s = do
  let zone = "Europe/Zurich"
  htDate <- getDateStringFromCalendarDateTimeLeniently zone y m d h mm s
  date <- getDateStringFromCmdDateString zone y m d h mm s
  assertEqual "date mismatch" date htDate

-- helper functions

getDateStringFromCalendarDateTimeLeniently :: String -> Year -> G.Month G.Gregorian -> DayOfMonth -> Hour -> Minute -> Second -> IO String
getDateStringFromCalendarDateTimeLeniently tz y m d h mm s = do
  zone <- timeZone tz
  let cdt = at <$> G.calendarDate d m y <*> localTime h mm s 0
  let zdt = flip fromCalendarDateTimeLeniently zone <$> cdt
  return . maybe "<INVALID>" toString $ zdt

getDateStringFromCmdDateString :: String -> Int -> G.Month G.Gregorian -> Int -> Int -> Int -> Int -> IO String
getDateStringFromCmdDateString tz y m d h mm sec = readCreateProcess process ""
  where
    process = proc' { env = Just env' }
    proc' = proc "mk_date" ["-a", "l", datetime]
    m' = (+1) . fromEnum $ m
    date = intercalate "-" [show y, show m', show d]
    time = intercalate ":" [show h, show mm, show sec]
    datetime = "--datetime=" ++ date ++ " " ++ time
    env' = [("TZ", tz)]

showDD :: Int -> String
showDD x = if x < 10 then "0" ++ show x else show x

toString :: ZonedDateTime G.Gregorian -> String
toString zdt = date ++ " " ++ time ++ " " ++ tzAbbr ++ "\n"
  where
    lt = toLocalTime zdt
    cd = toCalendarDate zdt
    tzAbbr = zoneAbbreviation zdt
    showMonth = showDD . (+1) . fromEnum
    date = intercalate "-" [show . get year $ cd, showMonth . month $ cd, showDD . get day $ cd]
    time = intercalate ":" [showDD . get hour $ lt, showDD . get minute $ lt, showDD . get second $ lt]

get :: ((s -> Const s c) -> a -> Const t b) -> a -> t
get l = getConst . l Const