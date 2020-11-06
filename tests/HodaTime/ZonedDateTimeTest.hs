module HodaTime.ZonedDateTimeTest
(
  zonedDateTimeTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Monadic (monadicIO, run)
import qualified Test.QuickCheck.Monadic as QCM
import Test.Tasty.HUnit
import Control.Monad (when, join)
import qualified System.Info as SysInfo
import Data.Maybe (catMaybes)

import HodaTime.Util
import Data.HodaTime.ZonedDateTime (fromCalendarDateTimeStrictly, fromCalendarDateTimeLeniently, fromCalendarDateTimeAll, toCalendarDateTime, fromInstant, toInstant, zoneAbbreviation, ZonedDateTime) -- remove ZonedDateTime
import Data.HodaTime.TimeZone (timeZone)
import Data.HodaTime.TimeZone.Internal (fixedOffsetZone, TZIdentifier(Zone), TimeZone(..))
import Data.HodaTime.Calendar.Gregorian (Month(..))
import qualified Data.HodaTime.Calendar.Gregorian as G
import Data.HodaTime.LocalTime (localTime)
import Data.HodaTime.CalendarDateTime (at)

zonedDateTimeTests :: TestTree
zonedDateTimeTests = testGroup "ZonedDateTime Tests" [qcProps, unitTests]

-- top level tests

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [calDateProps, toInstantProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
    lenientZoneTransitionUnits, allZoneTransitionUnits
  ]

-- properties

calDateProps :: TestTree
calDateProps = testGroup "CalendarDateTime conversion"
  [
     QC.testProperty "CalendarDateTime -> ZonedDateTime -> CalendarDateTime == id" $ testCalToZonedIdentity "UTC"
  ]
  where
    testCalToZonedIdentity zone (RandomStandardDate y mon d) (RandomTime h m s) = monadicIO $ do
      tz <- run (timeZone zone)
      let cdt = at <$> G.calendarDate d (toEnum mon) y <*> localTime h m s 0
      let zdt = join $ flip fromCalendarDateTimeStrictly tz <$> cdt
      let cdt' = toCalendarDateTime <$> zdt
      QCM.assert $ cdt == cdt'

toInstantProps :: TestTree
toInstantProps = testGroup "Instant conversion"
  [
     QC.testProperty "Instant -> ZonedDateTime -> Instant == id in UTC" $ testInstantToZonedIdentity "UTC"
    ,QC.testProperty "Instant -> ZonedDateTime -> Instant == id in Europe/Stockholm" $ testInstantToZonedIdentity "Europe/Stockholm"
    ,QC.testProperty "Instant -> ZonedDateTime -> Instant == id in fixedOffset TZ" $ testInstantToZonedIdentityFixed
  ]
  where
    testInstantToZonedIdentity zone i = monadicIO $ do
      tz <- run (timeZone zone)
      let zdt = fromInstant i tz :: ZonedDateTime G.Gregorian
      let i' = toInstant zdt
      run $ when (i /= i') (print $ "i = " <> show i <> " != " <> show i' <> " = i'")
      QCM.assert $ i == i'

    testInstantToZonedIdentityFixed o i = monadicIO $ do
      let (utcM, calDateM, _) = fixedOffsetZone "Fixed" o
      let tz = TimeZone (Zone "Fixed") utcM calDateM
      let zdt = fromInstant i tz :: ZonedDateTime G.Gregorian
      let i' = toInstant zdt

      QCM.assert $ i == i'

lenientZoneTransitionUnits :: TestTree
lenientZoneTransitionUnits = testGroup "fromCalendarDateTimeLeniently"
  [
     testCase "March 26 2017 2:10:15.30 -> March 26 2017 3:10:15.30 CEST" $ ensureHour startZone resultZone 26 March 2017 3
    ,testCase "October 29 2017 2:10:15.30 -> October 29 2017 2:10:15.30 CEST" $ ensureHour startZone resultZone 29 October 2017 2
    ,testCase "March 27 2039 2:10:15.30 -> March 27 2039 3:10:15.30 CEST" $ ensureHour startZone resultZone 27 March 2039 3
    ,testCase "October 30 2039 2:10:15.30 -> October 30 2039 2:10:15.30 CEST" $ ensureHour startZone resultZone 30 October 2039 2
  ]
  where
    startZone = if SysInfo.os == "mingw32" then "W. Europe Standard Time" else "Europe/Zurich"
    resultZone = if SysInfo.os == "mingw32" then "W. Europe Daylight Time" else "CEST"
    toLocalTime h = localTime h 10 15 30
    mkDate zone d m y = do
      tz <- timeZone zone
      let cdt = at <$> G.calendarDate d m y <*> toLocalTime 2
      return $ flip fromCalendarDateTimeLeniently tz <$> cdt
    ensureHour zone expectedAbbr d m y h = do
      zdt <- mkDate zone d m y
      let abbr = maybe "<INVALID>" zoneAbbreviation zdt
      let cdt = toCalendarDateTime <$> zdt
      let cdtExpected = at <$> G.calendarDate d m y <*> toLocalTime h
      assertEqual "Zone abbreviation" expectedAbbr abbr
      assertEqual "" cdtExpected cdt

allZoneTransitionUnits :: TestTree
allZoneTransitionUnits = testGroup "fromCalendarDateTimeAll"
  [
     testCase "March 26 2017 2:10:15.30 -> []" $ ensureHours startEuZone [] 26 March 2017 []
    ,testCase "October 29 2017 2:10:15.30 -> [October 29 2017: 2:10:15.30 CEST, 2:10:15.30 CET]" $ ensureHours startEuZone [summerEuZone, normEuZone] 29 October 2017 [2,2]
    ,testCase "March 27 2039 2:10:15.30 -> []" $ ensureHours startEuZone [] 27 March 2039 []
    ,testCase "October 30 2039 2:10:15.30 -> [October 30 2039: 2:10:15.30 CEST, 2:10:15.30 CET]" $ ensureHours startEuZone [summerEuZone, normEuZone] 30 October 2039 [2,2]

    ,testCase "April 3 2005 2:10:15.30 -> []" $ ensureHours startUsZone [] 3 April 2005 []
    ,testCase "October 30 2005 1:10:15.30 -> [October 30 2005: 1:10:15.30 CDT, 1:10:15.30 CST]" $ ensureHours' 1 startUsZone [summerUsZone, normUsZone] 30 October 2005 [1,1]
    ,testCase "April 2 2006 2:10:15.30 -> []" $ ensureHours startUsZone [] 2 April 2006 []
    ,testCase "October 29 2006 1:10:15.30 -> [October 29 2006: 1:10:15.30 CDT, 1:10:15.30 CST]" $ ensureHours' 1 startUsZone [summerUsZone, normUsZone] 29 October 2006 [1,1]
    ,testCase "March 11 2007 2:10:15.30 -> []" $ ensureHours startUsZone [] 11 March 2007 []
    ,testCase "November 4 2007 1:10:15.30 -> [November 4 2007: 1:10:15.30 CDT, 1:10:15.30 CST]" $ ensureHours' 1 startUsZone [summerUsZone, normUsZone] 4 November 2007 [1,1]
    ,testCase "March 9 2008 2:10:15.30 -> []" $ ensureHours startUsZone [] 9 March 2008 []
    ,testCase "November 2 2008 1:10:15.30 -> [November 2 2008: 1:10:15.30 CDT, 1:10:15.30 CST]" $ ensureHours' 1 startUsZone [summerUsZone, normUsZone] 2 November 2008 [1,1]
    ,testCase "March 13 2039 2:10:15.30 -> []" $ ensureHours startUsZone [] 13 March 2039 []
    ,testCase "November 6 2039 1:10:15.30 -> [November 6 2039: 1:10:15.30 CDT, 1:10:15.30 CST]" $ ensureHours' 1 startUsZone [summerUsZone, normUsZone] 6 November 2039 [1,1]
  ]
  where
    startEuZone = if SysInfo.os == "mingw32" then "W. Europe Standard Time" else "Europe/Zurich"
    startUsZone = if SysInfo.os == "mingw32" then "Central Standard Time" else "US/Central"
    normEuZone = if SysInfo.os == "mingw32" then "W. Europe Standard Time" else "CET"
    normUsZone = if SysInfo.os == "mingw32" then "Central Standard Time" else "CST"
    summerEuZone = if SysInfo.os == "mingw32" then "W. Europe Daylight Time" else "CEST"
    summerUsZone = if SysInfo.os == "mingw32" then "Central Daylight Time" else "CDT"
    toLocalTime h = localTime h 10 15 30
    mkDates h zone d m y = do
      tz <- timeZone zone
      let cdt = at <$> G.calendarDate d m y <*> toLocalTime h
      return $ flip fromCalendarDateTimeAll tz <$> cdt
    ensureHours zone expectedAbbrs d m y hs = ensureHours' 2 zone expectedAbbrs d m y hs
    ensureHours' hour zone expectedAbbrs d m y hs = do
      zdts <- mkDates hour zone d m y
      let abbrs = maybe [] (zoneAbbreviation <$>) zdts
      let cdts = maybe [] (toCalendarDateTime <$>) zdts
      let cdtExpecteds = catMaybes $ (\x -> at <$> G.calendarDate d m y <*> toLocalTime x) <$> hs
      assertEqual "Zone abbreviation" expectedAbbrs abbrs
      assertEqual "" cdtExpecteds cdts
