import Test.Tasty

import HodaTime.InstantTest
import HodaTime.DurationTest
import HodaTime.OffsetTest
import HodaTime.LocalTimeTest
import HodaTime.Calendar.GregorianTest
import HodaTime.CalendarDateTimeTest
import HodaTime.ZonedDateTimeTest
import HodaTime.PatternTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [instantTests, durationTests, offsetTests, localTimeTests, gregorianTests, calendarDateTimeTests, zonedDateTimeTests, patternTests]

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
