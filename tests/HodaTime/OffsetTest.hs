module HodaTime.OffsetTest
(
  offsetTests
)
where

import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit ()
import HodaTime.Util
import Data.HodaTime.Offset

offsetTests :: TestTree
offsetTests = testGroup "Offset Tests" [scProps, qcProps, unitTests]

-- top level tests

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)" [mathPropSC]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [secondProps, mathProps, lensProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
  ]

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

lensProps :: TestTree
lensProps = testGroup "Lens"
  [
     QC.testProperty "get seconds offset" $ testGet seconds _1
    ,QC.testProperty "get minutes offset" $ testGet minutes _2
    ,QC.testProperty "get hours offset" $ testGet hours _3
    ,QC.testProperty "modify seconds offset" $ testF (modify . (+)) seconds _1 (+) 5
    ,QC.testProperty "modify minutes offset" $ testF (modify . (+)) minutes _2 (+) 5
    ,QC.testProperty "modify hours offset" $ testF (modify . (+)) hours _3 (+) 5
    ,QC.testProperty "set seconds offset" $ testF set seconds _1 const 5
    ,QC.testProperty "set minutes offset" $ testF set minutes _2 const 5
    ,QC.testProperty "set hours offset" $ testF set hours _3 const 5
  ]
  where
    offset :: Int -> Int -> Int -> Offset   -- Only needed so the compiler can decide which concreate type to use
    offset s m h = fromSeconds s `addClamped` fromMinutes m `addClamped` fromHours h
    offsetEq (s, m, h) off = get seconds off == s && get minutes off == m && get hours off == h
    _1 f (a,b,c) = (\a' -> (a',b,c)) <$> f a
    _2 f (a,b,c) = (\b' -> (a,b',c)) <$> f b
    _3 f (a,b,c) = (\c' -> (a,b,c')) <$> f c
    testGet l l' (RandomOffset h m s) = get l (offset s m h) == get l' (s, m, h)
    testF f l l' g n (RandomOffset h m s) = h < 18 - n && s < 60 - n && m < 60 - n QC.==> offsetEq (modify (g n) l' (s,m,h)) $ f n l (offset s m h)

-- helper functions

test :: (Int -> Offset) -> (Offset -> Offset -> Offset) -> (Int -> Int -> Int) -> Int -> Int -> Bool
test f g h x y = f x `g` f y == f (h x y)

test_from :: (Int -> Offset) -> (Int -> Offset) -> Int -> Int -> Bool
test_from g f y x = f x == g (y*x)
