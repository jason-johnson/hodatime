import Test.Tasty

import HodaTime.InstantTest
import HodaTime.DurationTest
import HodaTime.OffsetTest
import HodaTime.LocalTimeTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [instantTests, durationTests, offsetTests, localTimeTests]

{-
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]
-}
