{-# LANGUAGE FlexibleInstances #-}

module HodaTime.Calendar.GregorianTest
(
  gregorianTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Maybe (fromJust)

import HodaTime.Util
import Data.HodaTime.Calendar (day, monthl, year)
import Data.HodaTime.Calendar.Gregorian (calendarDate, Month(..), Gregorian)

instance Arbitrary (Month Gregorian) where
  arbitrary = do
    x <- choose (0,11)
    return $ toEnum x

gregorianTests :: TestTree
gregorianTests = testGroup "Gregorian Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [lensProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [leapUnits]

lensProps :: TestTree
lensProps = testGroup "Lens"
  [
     QC.testProperty "first day not changed by month math" $ testMonthAdd 1
    ,QC.testProperty "mid day not changed by month math" $ testMonthAdd 15
  ]
  where
    mkcd d m = fromJust . calendarDate d m
    testMonthAdd d (Positive y) m add = y < 400 QC.==> get day (modify (+ add) monthl $ mkcd d m (y + 1900)) == d  -- NOTE: We fix the year so we don't run out of tests

leapUnits :: TestTree
leapUnits = testGroup "Rollover"
  [
     testCase "31-January-2000 + 2M == 31-March-2000" $ modify (+2) monthl <$> janEnd @?= calendarDate 31 March 2000
    ,testCase "31-January-2000 + 1M == 29-February-2000" $ modify (+1) monthl <$> janEnd @?= calendarDate 29 February 2000
    ,testCase "29-February-2000 + 1Y == 28-February-2001" $ modify (+1) year <$> leapFeb @?= calendarDate 28 February 2001
  ]
    where
      janEnd = calendarDate 31 January 2000
      leapFeb = calendarDate 29 February 2000