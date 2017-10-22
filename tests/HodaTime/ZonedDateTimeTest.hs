module HodaTime.ZonedDateTimeTest
(
  zonedDateTimeTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Maybe (fromJust)

import HodaTime.Util
import Data.HodaTime.ZonedDateTime ()

zonedDateTimeTests :: TestTree
zonedDateTimeTests = testGroup "ZonedDateTime Tests" [scProps, qcProps, unitTests]

-- top level tests

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)" []

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" []

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
  ]

calDateProps :: TestTree
calDateProps = testGroup "CalendarDateTime conversion"
  [
     QC.testProperty "fromSeconds (x * 60) == fromMinutes x" $ testS fromMinutes mins
    ,QC.testProperty "fromSeconds (x * 60 * 60) == fromHours x" $ testS fromHours hrs
  ]
  where
    testS = test_from fromSeconds
    mins = 60
    hrs = mins*60