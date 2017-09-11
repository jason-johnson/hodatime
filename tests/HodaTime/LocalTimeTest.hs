module HodaTime.LocalTimeTest
(
  localTimeTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromJust)

import HodaTime.Util
import Data.HodaTime.LocalTime (hours, minutes, seconds, localTime)

localTimeTests :: TestTree
localTimeTests = testGroup "LocalTime Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [lensProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [rolloverUnits]

-- properties

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
    mkTime h m s = fromJust . localTime h m s $ 0    -- We are already controlling that only valid values will be passed in
    offsetEq (s, m, h) off = get seconds off == s && get minutes off == m && get hours off == h
    _1 f (a,b,c) = (\a' -> (a',b,c)) <$> f a
    _2 f (a,b,c) = (\b' -> (a,b',c)) <$> f b
    _3 f (a,b,c) = (\c' -> (a,b,c')) <$> f c
    testGet l l' (Positive s, Positive m, Positive h) = h < 23 && s < 60 && m < 60 QC.==> get l (mkTime h m s) == get l' (s, m, h)
    testF f l l' g n (Positive s, Positive m, Positive h) = h < 23 - n && s < 60 - n && m < 60 - n QC.==> offsetEq (modify (g n) l' (s,m,h)) $ f n l (mkTime h m s)

rolloverUnits :: TestTree
rolloverUnits = testGroup "Rollover"
  [
     testCase "22:57:57 + 2s == 22:57:59" $ modify (+2) seconds <$> time @?= localTime 22 57 59 0
    ,testCase "22:57:57 + 5s == 22:58:02" $ modify (+5) seconds <$> time @?= localTime 22 58 2 0
    ,testCase "22:57:57 + 2m == 22:59:57" $ modify (+2) minutes <$> time @?= localTime 22 59 57 0
    ,testCase "22:57:57 + 5m == 23:02:57" $ modify (+5) minutes <$> time @?= localTime 23 02 57 0
    ,testCase "22:57:57 + 1h == 23:57:57" $ modify (+1) hours <$> time @?= localTime 23 57 57 0
    ,testCase "22:57:57 + 3h == 01:57:57" $ modify (+3) hours <$> time @?= localTime 1 57 57 0
    ,testCase "22:57:57 + 3723s == 00:00:00" $ modify (+3723) seconds <$> time @?= localTime 0 0 0 0
    ,testCase "22:57:57 + 3725s == 00:00:02" $ modify (+3725) seconds <$> time @?= localTime 0 0 2 0
  ]
  where
    time = localTime 22 57 57 0
