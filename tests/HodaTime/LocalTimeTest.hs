module HodaTime.LocalTimeTest
(
  instantTests
)
where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch, fromInstant)
import Data.HodaTime.LocalTime (hour, minute, second)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.LocalTime (todHour, todMin, todSec, hoursToTimeZone, utcToLocalTime, LocalTime(..))

instantTests :: TestTree
instantTests = testGroup "LocalTime Tests" [scProps, qcProps, unitTests]

scProps = testGroup "(checked by SmallCheck)"
  [
    SC.testProperty "sort == sort . reverse" $ \list -> (list :: [Int]) == reverse list
  ]

qcProps = testGroup "(checked by SmallCheck)"
  [
  ]

unitTests = testGroup "Unit tests"
  [
  ]
