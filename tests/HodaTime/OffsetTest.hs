module HodaTime.OffsetTest
(
  offsetTests
)
where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.HodaTime.Offset

offsetTests :: TestTree
offsetTests = testGroup "Offset Tests" [scProps, qcProps, unitTests]

-- top level tests

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)" [mathPropSC]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [secondProps, mathProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
  ]

-- properties

mathPropSC :: TestTree
mathPropSC = localOption (SmallCheckDepth 18) $ testGroup "Math"  -- NOTE: Max offset size is 18/-18 so we set the depth to make sure everything in that range is tested
  [
     SC.testProperty "fromHours x `add` fromHours y == fromHours (x+y)" $ test fromHours add (+)
    ,SC.testProperty "fromHours x `minus` fromHours y == fromHours (x-y)" $ test fromHours minus (-)
  ]

secondProps :: TestTree
secondProps = testGroup "Seconds conversion"
  [
     QC.testProperty "fromSeconds (x * 60) == fromMinutes x" $ testS fromMinutes mins
    ,QC.testProperty "fromSeconds (x * 60 * 60) == fromHours x" $ testS fromHours hrs
  ]
  where
    testS = test_from fromSeconds
    mins = 60
    hrs = mins*60

mathProps :: TestTree
mathProps = testGroup "Math"
  [
     QC.testProperty "fromSeconds x `add` fromSeconds y == fromSeconds (x+y)" $ test fromSeconds add (+)
    ,QC.testProperty "fromSeconds x `minus` fromSeconds y == fromSeconds (x-y)" $ test fromSeconds minus (-)
    ,QC.testProperty "fromMinutes x `add` fromMinutes y == fromMinutes (x+y)" $ test fromMinutes add (+)
    ,QC.testProperty "fromMinutes x `minus` fromMinutes y == fromMinutes (x-y)" $ test fromMinutes minus (-)
  ]

-- helper functions

test :: (Int -> Offset) -> (Offset -> Offset -> Offset) -> (Int -> Int -> Int) -> Int -> Int -> Bool
test f g h x y = f x `g` f y == f (h x y)

test_from :: (Int -> Offset) -> (Int -> Offset) -> Int -> Int -> Bool
test_from g f y x = f x == g (y*x)
