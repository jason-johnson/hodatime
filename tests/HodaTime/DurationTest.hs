module HodaTime.DurationTest
(
  durationTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.HodaTime.Duration

-- Tests

durationTests :: TestTree
durationTests = testGroup "Duration Tests" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [nanoSecProps, secondProps, dayProps, mathProps]


-- Properties

nanoSecProps :: TestTree
nanoSecProps = testGroup "Nanoseconds conversion"
  [
     QC.testProperty "fromNanoseconds (x * 1000) == fromMicroseconds x" $ test fromMicroseconds micro
    ,QC.testProperty "fromNanoseconds (x * 1000 * 1000) == fromMilliseconds x" $ test fromMilliseconds milli
    ,QC.testProperty "fromNanoseconds (x * 1000 * 1000 * 1000) == fromSeconds x" $ test fromSeconds sec
  ]
  where
    test = test_from fromNanoseconds
    micro = 1000
    milli = micro*1000
    sec = milli*1000

secondProps :: TestTree
secondProps = testGroup "Seconds conversion"
  [
     QC.testProperty "fromSeconds (x * 60) == fromMinutes x" $ test fromMinutes mins
    ,QC.testProperty "fromSeconds (x * 60 * 60) == fromHours x" $ test fromHours hours
    ,QC.testProperty "fromSeconds (x * 60 * 60 * 24) == fromStandardDays x" $ test fromStandardDays sdays
    ,QC.testProperty "fromSeconds (x * 60 * 60 * 24 * 7) == fromStandardWeeks x" $ test fromStandardWeeks sweeks
  ]
  where
    test = test_from fromSeconds
    mins = 60
    hours = mins*60
    sdays = hours*24
    sweeks = sdays*7

dayProps :: TestTree
dayProps = testGroup "Days conversion"
  [
    QC.testProperty "fromStandardDays (x * 7) == fromStandardWeeks x" $ test fromStandardWeeks sweeks
  ]
  where
    test = test_from fromStandardDays
    sweeks = 7

mathProps :: TestTree
mathProps = testGroup "Math"
  [
     QC.testProperty "fromNanoseconds x `add` fromNanoseconds y == fromNanoseconds (x+y)" $ test add (+)
    ,QC.testProperty "fromNanoseconds x `minus` fromNanoseconds y == fromNanoseconds (x-y)" $ test minus (-)
  ]
  where
    test f g (Positive x) (Positive y) = fromNanoseconds x `f` fromNanoseconds y == fromNanoseconds (g x y)

-- helper functions

test_from :: (Int -> Duration) -> (Int -> Duration) -> Int -> Int -> Bool
test_from g f y x = f x == g (y*x)
