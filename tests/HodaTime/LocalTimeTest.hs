module HodaTime.LocalTimeTest
(
  localTimeTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Maybe (fromJust)

import HodaTime.Util (RandomTime(..))
import Data.HodaTime.LocalTime (localTime, LocalTime, hour, minute, second)
import Data.HodaTime.Period (applyPeriod, hours, minutes, seconds)

localTimeTests :: TestTree
localTimeTests = testGroup "LocalTime Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [accessorProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [rolloverUnits]

-- properties

accessorProps :: TestTree
accessorProps = testGroup "Accessors"
  [
     QC.testProperty "reads constructed components" $ \(RandomTime h m s) ->
       let value = mkTime h m s in (hour value, minute value, second value) == (h, m, s)
  ]
  where
    mkTime h m s = fromJust (localTime h m s 0)

rolloverUnits :: TestTree
rolloverUnits = testGroup "Rollover"
  [
    testCase "22:57:57 + 2s == 22:57:59" $ applyPeriod (seconds 2) <$> time @?= localTime 22 57 59 0
      ,testCase "22:57:57 + 5s == 22:58:02" $ applyPeriod (seconds 5) <$> time @?= localTime 22 58 2 0
      ,testCase "22:57:57 + 2m == 22:59:57" $ applyPeriod (minutes 2) <$> time @?= localTime 22 59 57 0
      ,testCase "22:57:57 + 5m == 23:02:57" $ applyPeriod (minutes 5) <$> time @?= localTime 23 02 57 0
      ,testCase "22:57:57 + 1h == 23:57:57" $ applyPeriod (hours 1) <$> time @?= localTime 23 57 57 0
      ,testCase "22:57:57 + 3h == 01:57:57" $ applyPeriod (hours 3) <$> time @?= localTime 1 57 57 0
      ,testCase "22:57:57 + 3723s == 00:00:00" $ applyPeriod (seconds 3723) <$> time @?= localTime 0 0 0 0
      ,testCase "22:57:57 + 3725s == 00:00:02" $ applyPeriod (seconds 3725) <$> time @?= localTime 0 0 2 0
  ]
  where
    time :: Maybe LocalTime
    time = localTime 22 57 57 0
