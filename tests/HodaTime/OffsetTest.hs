module HodaTime.OffsetTest
(
  offsetTests
)
where

import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import HodaTime.Util
import Data.HodaTime.Offset

offsetTests :: TestTree
offsetTests = testGroup "Offset Tests" [scProps, qcProps, unitTests]

-- top level tests

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)" [mathPropSC]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [secondProps, mathProps, componentProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
     testCase "-01:30 -> hours -1"                          $ hours (fromSeconds (-5400 :: Int)) @?= (-1)
    ,testCase "-01:30 -> minutes -30"                       $ minutes (fromSeconds (-5400 :: Int)) @?= (-30)
    ,testCase "-00:30 -> (hours,minutes,seconds) == (0,-30,0)" $ triple (fromSeconds (-1800 :: Int)) @?= (0, -30, 0)
    ,testCase "-01:01:01 -> (-1,-1,-1)"                     $ triple (fromSeconds (-3661 :: Int)) @?= (-1, -1, -1)
    ,testCase "+02:15 -> (2,15,0)"                          $ triple (fromSeconds (2*3600 + 15*60 :: Int)) @?= (2, 15, 0)
  ]
  where
    triple o = (hours o, minutes o, seconds o)

-- properties

mathPropSC :: TestTree
mathPropSC = localOption (SC.SmallCheckDepth 18) $ testGroup "Math"  -- NOTE: Max offset size is 18/-18 so we set the depth to make sure everything in that range is tested
  [
     SC.testProperty "fromHours x `addClamped` fromHours y == fromHours (x+y)" $ test fromHours addClamped (+)
    ,SC.testProperty "fromHours x `minusClamped` fromHours y == fromHours (x-y)" $ test fromHours minusClamped (-)
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
     QC.testProperty "fromSeconds x `addClamped` fromSeconds y == fromSeconds (x+y)" $ test fromSeconds addClamped (+)
    ,QC.testProperty "fromSeconds x `minusClamped` fromSeconds y == fromSeconds (x-y)" $ test fromSeconds minusClamped (-)
    ,QC.testProperty "fromMinutes x `addClamped` fromMinutes y == fromMinutes (x+y)" $ test fromMinutes addClamped (+)
    ,QC.testProperty "fromMinutes x `minusClamped` fromMinutes y == fromMinutes (x-y)" $ test fromMinutes minusClamped (-)
  ]

componentProps :: TestTree
componentProps = testGroup "Components"
  [
     QC.testProperty "components reconstruct the offset" $ \(RandomOffset h m s) ->
        let t = h*3600 + m*60 + s; o = fromSeconds t in hours o * 3600 + minutes o * 60 + seconds o == t
    ,QC.testProperty "each component carries the offset's sign (or is zero)" $ \(RandomOffset h m s) ->
        let t = h*3600 + m*60 + s; o = fromSeconds t; ok x = x == 0 || signum x == signum t
        in ok (hours o) && ok (minutes o) && ok (seconds o)
    ,QC.testProperty "sub-hour components stay under 60" $ \(RandomOffset h m s) ->
        let o = fromSeconds (h*3600 + m*60 + s) in abs (minutes o) < 60 && abs (seconds o) < 60
  ]

-- helper functions

test :: (Int -> Offset) -> (Offset -> Offset -> Offset) -> (Int -> Int -> Int) -> Int -> Int -> Bool
test f g h x y = f x `g` f y == f (h x y)

test_from :: (Int -> Offset) -> (Int -> Offset) -> Int -> Int -> Bool
test_from g f y x = f x == g (y*x)
