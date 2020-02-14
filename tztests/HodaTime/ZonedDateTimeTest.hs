module HodaTime.ZonedDateTimeTest
(
  zonedDateTimeTests
)
where

import Test.Tasty
import Test.Tasty.HUnit

import Data.HodaTime.ZonedDateTime (ZonedDateTime, fromCalendarDateTimeLeniently, fromInstant, toCalendarDate, toLocalTime, inDst, zoneAbbreviation)
import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch)
import Data.HodaTime.TimeZone (timeZone)
import qualified Data.HodaTime.Calendar.Gregorian as G
import Data.HodaTime.Calendar.Gregorian (Month(..))
import Data.HodaTime.LocalTime (localTime, hour, minute, second, Hour, Minute, Second)
import Data.HodaTime.CalendarDateTime (at, year, month, day, CalendarDate, Year, DayOfMonth)

import qualified System.Info as SysInfo
import System.Process (readCreateProcess, proc, env)
import Data.List (intercalate)
import Control.Applicative (Const(..))

zonedDateTimeTests :: TestTree
--zonedDateTimeTests = testGroup "ZonedDateTime Tests" [unitTests, fromCalendarDateTimeLeniently2017Tests, fromCalendarDateTimeLeniently2039Tests, fromInstantTests]
zonedDateTimeTests = testGroup "ZonedDateTime Tests" [unitTests, fromInstantTests]

-- top level tests

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
  ]

fromCalendarDateTimeLeniently2017Tests :: TestTree
fromCalendarDateTimeLeniently2017Tests = testGroup "fromCalendarDateTimeLeniently 2017 tests" $ test_fromCalendarDateTimeLenientlyFor 2017

fromCalendarDateTimeLeniently2039Tests :: TestTree
fromCalendarDateTimeLeniently2039Tests = testGroup "fromCalendarDateTimeLeniently 2039 tests" $ test_fromCalendarDateTimeLenientlyFor 2039

fromInstantTests :: TestTree
fromInstantTests = testGroup "fromInstant tests" test_fromInstant

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

test_fromInstant :: [TestTree]
test_fromInstant = do
  let secs = 60 * 60 * 24
  let start = 1445734400
  let end = start + secs
  epoch <- [start..end]
  let caseStr = show epoch
  return . testCase caseStr $ test_getEpochs epoch

-- helper functions

test_getEpochs :: Int -> Assertion
test_getEpochs e = do
  let zone = if SysInfo.os == "mingw32" then "W. Europe Standard Time" else "Europe/Zurich"
  htDate <- getDateStringFromInstant zone e
  date <- getDateStringFromCmdEpoch zone e
  assertEqual "date mismatch" date htDate

test_getDates :: Int -> G.Month G.Gregorian -> Int -> Int -> Int -> Int -> Assertion
test_getDates y m d h mm s = do
  let zone = if SysInfo.os == "mingw32" then "W. Europe Standard Time" else "Europe/Zurich"
  htDate <- getDateStringFromCalendarDateTimeLeniently zone y m d h mm s
  date <- getDateStringFromCmdDateString zone y m d h mm s
  assertEqual "date mismatch" date htDate

getDateStringFromCalendarDateTimeLeniently :: String -> Year -> G.Month G.Gregorian -> DayOfMonth -> Hour -> Minute -> Second -> IO String
getDateStringFromCalendarDateTimeLeniently tz y m d h mm s = do
  zone <- timeZone tz
  let cdt = at <$> G.calendarDate d m y <*> localTime h mm s 0
  let zdt = flip fromCalendarDateTimeLeniently zone <$> cdt
  return . maybe "<INVALID>" toString $ zdt

getDateStringFromCmdDateString :: String -> Int -> G.Month G.Gregorian -> Int -> Int -> Int -> Int -> IO String
getDateStringFromCmdDateString tz y m d h mm sec = readMkDateProcess env' args
  where
    args = ["-a", "l", datetime]
    m' = (+1) . fromEnum $ m
    date = intercalate "-" [show y, show m', show d]
    time = intercalate ":" [show h, show mm, show sec]
    datetime = "--datetime=" ++ date ++ " " ++ time
    env' = [("TZ", tz)]

getDateStringFromInstant :: String -> Int -> IO String
getDateStringFromInstant tz epoch = do
  zone <- timeZone tz
  let instant = fromSecondsSinceUnixEpoch epoch
  let zdt = fromInstant instant zone
  return . toString $ zdt

getDateStringFromCmdEpoch :: String -> Int -> IO String
getDateStringFromCmdEpoch tz epoch = readMkDateProcess env' args
  where
    env' = [("TZ", tz)]
    args = ["-e", show epoch]

readMkDateProcess :: [(String, String)] -> [String] -> IO String
readMkDateProcess env' cmdArgs = readCreateProcess process ""
  where
    process = proc' { env = Just env' }
    proc' = proc "mk_date" cmdArgs

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