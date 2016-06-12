module HodaTime.LocalTimeTest
(
  localTimeTests
)
where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.HodaTime.LocalTime (hour, minute, second)

localTimeTests :: TestTree
localTimeTests = testGroup "LocalTime Tests" [scProps, qcProps, unitTests]

scProps = testGroup "(checked by SmallCheck)" []

qcProps = testGroup "(checked by SmallCheck)" []

unitTests = testGroup "Unit tests" []
