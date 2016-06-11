module HodaTime.DurationTest
(
  durationTests
)
where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.HodaTime.Duration

durationTests :: TestTree
durationTests = testGroup "Duration Tests" [scProps, qcProps, unitTests]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
  [
  ]

qcProps :: TestTree
qcProps = testGroup "(checked by SmallCheck)"
  [
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
     testCase "test_NanosecondDuration" test_MicrosecondDuration
    ,testCase "test_MillisecondDuration" test_MillisecondDuration
    ,testCase "test_NanosToSecondDuration" test_NanosToSecondDuration
  ]

test_MicrosecondDuration :: Assertion
test_MicrosecondDuration = do
  let
    ns = fromNanoseconds 1000
    ms = fromMicroseconds 1
  assertEqual "micro seconds: " ns ms

test_MillisecondDuration :: Assertion
test_MillisecondDuration = do
  let
    ns = fromNanoseconds 1000000
    ms = fromMilliseconds 1
  assertEqual "milli seconds: " ns ms

-- NOTE: Here the nano seconds entry will be empty, both will convert to seconds
test_NanosToSecondDuration :: Assertion
test_NanosToSecondDuration = do
  let
    ns = fromNanoseconds 1000000000
    ms = fromSeconds 1
  assertEqual "seconds: " ns ms
