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
import Control.Monad (join)

import HodaTime.Util
import Data.HodaTime.ZonedDateTime (fromCalendarDateTimeStrictly, fromCalendarDateTimeLeniently, toLocalDateTime)
import Data.HodaTime.TimeZone (timeZone)
import Data.HodaTime.Calendar.Gregorian (Month(..))
import qualified Data.HodaTime.Calendar.Gregorian as G
import Data.HodaTime.LocalTime (localTime)
import Data.HodaTime.CalendarDateTime (at)

zonedDateTimeTests :: TestTree
zonedDateTimeTests = testGroup "ZonedDateTime Tests" [qcProps, unitTests]

-- top level tests

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [calDateProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
    zoneTransitionUnits
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
      let cdt' = toLocalDateTime <$> zdt
      QCM.assert $ cdt == cdt'

zoneTransitionUnits :: TestTree
zoneTransitionUnits = testGroup "Zone transition"
  [
     testCase "March 26 2017 2:10:15.30 -> March 26 2017 3:10:15.30" $ ensureHour "Europe/Zurich" 26 March 2017 3
    ,testCase "October 29 2017 2:10:15.30 -> October 29 2017 2:10:15.30" $ ensureHour "Europe/Zurich" 29 October 2017 2
    ,testCase "March 27 2039 2:10:15.30 -> March 27 2039 2:10:15.30" $ ensureHour "Europe/Zurich" 27 March 2039 3
    ,testCase "October 30 2039 2:10:15.30 -> October 30 2039 2:10:15.30" $ ensureHour "Europe/Zurich" 30 October 2039 2
  ]
  where
    toLocalTime h = localTime h 10 15 30
    ensureHour zone d m y h = do
      cdt <- mkDate zone d m y
      let cdtExpected = at <$> G.calendarDate d m y <*> toLocalTime h
      assertEqual "" cdtExpected cdt
    mkDate zone d m y = do
      tz <- timeZone zone
      let cdt = at <$> G.calendarDate d m y <*> toLocalTime 2
      let zdt = flip fromCalendarDateTimeLeniently tz <$> cdt
      return $ toLocalDateTime <$> zdt