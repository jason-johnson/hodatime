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
qcProps = testGroup "(checked by QuickCheck)" [mathProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
  ]

-- properties

mathPropSC :: TestTree
mathPropSC = localOption (SmallCheckDepth 18) $ testGroup "Math"  -- NOTE: Max offset size is 18/-18 so we set the depth to make sure everything in that range is tested
  [
     SC.testProperty "fromHours x `add` fromHours y == fromHours (x+y)" $ test fromHours add plus
    ,SC.testProperty "fromHours x `minus` fromHours y == fromHours (x-y)" $ test fromHours minus subtr
  ]
  where
    test f g h x y = f x `g` f y == f (h x y)   -- TODO: move this to top level and fix the times.  This lets us get rid of plus and subtr

mathProps :: TestTree
mathProps = testGroup "Math"
  [
     QC.testProperty "fromSeconds x `add` fromSeconds y == fromSeconds (x+y)" $ test fromSeconds add plus
    ,QC.testProperty "fromSeconds x `minus` fromSeconds y == fromSeconds (x-y)" $ test fromSeconds minus subtr
    ,QC.testProperty "fromMinutes x `add` fromMinutes y == fromMinutes (x+y)" $ test fromMinutes add plus
    ,QC.testProperty "fromMinutes x `minus` fromMinutes y == fromMinutes (x-y)" $ test fromMinutes minus subtr
  ]
  where
    test f g h x y = f x `g` f y == f (h x y)       -- we could also give this function a signature and get rid of the 2 below

-- helper functions

-- NOTE: The following 2 functions only exist to lock in the type so the QC tests can work
plus :: Int -> Int -> Int
plus = (+)

subtr :: Int -> Int -> Int
subtr = (-)
