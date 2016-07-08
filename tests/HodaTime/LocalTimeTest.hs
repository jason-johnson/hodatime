module HodaTime.LocalTimeTest
(
  localTimeTests
)
where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromJust)

import Data.HodaTime.LocalTime (hours, minutes, seconds, fromTime)

localTimeTests :: TestTree
localTimeTests = testGroup "LocalTime Tests" [scProps, qcProps, unitTests]

scProps = testGroup "(checked by SmallCheck)" []

qcProps = testGroup "(checked by QuickCheck)" [lensProps]

lensProps :: TestTree
lensProps = testGroup "Lens"
  [
     QC.testProperty "get seconds offset" $ testGet seconds _1
    ,QC.testProperty "get minutes offset" $ testGet minutes _2
    ,QC.testProperty "get hours offset" $ testGet hours _3
    ,QC.testProperty "modify seconds offset" $ testF (modify . (+)) seconds _1 (+) 5
    ,QC.testProperty "modify minutes offset" $ testF (modify . (+)) minutes _2 (+) 5
    ,QC.testProperty "modify hours offset" $ testF (modify . (+)) hours _3 (+) 5
    ,QC.testProperty "set seconds offset" $ testF setL seconds _1 const 5
    ,QC.testProperty "set minutes offset" $ testF setL minutes _2 const 5
    ,QC.testProperty "set hours offset" $ testF setL hours _3 const 5
  ]
  where
    mkTime h m s = fromJust . fromTime h m s $ 0    -- We are already controlling that only valid values will be passed in
    offsetEq (s, m, h) off = get seconds off == s && get minutes off == m && get hours off == h
    _1 f (a,b,c) = (\a' -> (a',b,c)) <$> f a
    _2 f (a,b,c) = (\b' -> (a,b',c)) <$> f b
    _3 f (a,b,c) = (\c' -> (a,b,c')) <$> f c
    get l = getConst . l Const
    modify f l = runIdentity . l (Identity . f)
    setL v = modify (const v)
    testGet l l' (Positive s, Positive m, Positive h) = h < 23 && s < 60 && m < 60 QC.==> get l (mkTime h m s) == get l' (s, m, h)
    testF f l l' g n (Positive s, Positive m, Positive h) = h < 23 - n && s < 60 - n && m < 60 - n QC.==> offsetEq (modify (g n) l' (s,m,h)) $ f n l (mkTime h m s)

unitTests = testGroup "Unit tests" []
