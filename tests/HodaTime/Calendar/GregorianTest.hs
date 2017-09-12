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
import Data.Time.Calendar (fromGregorianValid, toGregorian)

import HodaTime.Util
import Data.HodaTime.Calendar (day, monthl, month, year, next, previous, dayOfWeek)
import Data.HodaTime.Calendar.Gregorian (calendarDate, Month(..), DayOfWeek(..), Gregorian)
import qualified Data.HodaTime.Calendar.Gregorian as G
import qualified Data.HodaTime.Calendar.Iso as Iso

instance Arbitrary (Month Gregorian) where
  arbitrary = do
    x <- choose (0,11)
    return $ toEnum x

instance Arbitrary (DayOfWeek Gregorian) where
  arbitrary = do
    x <- choose (0,6)
    return $ toEnum x

gregorianTests :: TestTree
gregorianTests = testGroup "Gregorian Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [constructorProps, lensProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [constructorUnits, lensUnits]

constructorProps :: TestTree
constructorProps = testGroup "Constructor"
  [
    QC.testProperty "same dates as Data.Time" $ testConstructor
  ]
    where
      areSame Nothing Nothing = True
      areSame (Just hdate) (Just date) =
        let
          (ty, tm, tday) = toGregorian date
        in get day hdate == tday && (convertMonth . month $ hdate) == tm && get year hdate == (fromIntegral ty)
      areSame _ _ = False
      convertMonth = succ . fromEnum
      testConstructor (Positive y) m (Positive d) = areSame (calendarDate d m y') (fromGregorianValid (fromIntegral y') (convertMonth m) d)
        where
          y' = 1900 + y

lensProps :: TestTree
lensProps = testGroup "Lens"
  [
     QC.testProperty "first day not changed by month math" $ testMonthAdd 1
    ,QC.testProperty "mid day not changed by month math" $ testMonthAdd 15
    ,QC.testProperty "dayOfWeek . next n dow $ date == dow" $ testNextDoW
    ,QC.testProperty "next n (dayOfWeek date) date == modify (+ n * 7) day date" $ testDirection next (+)
    ,QC.testProperty "previous n (dayOfWeek date) date == modify (- n * 7) day date" $ testDirection previous $ flip (-)
  ]
  where
    mkcd d m = fromJust . calendarDate d m
    testMonthAdd d (Positive y) m add = y < 400 QC.==> get day (modify (+ add) monthl $ mkcd d m (y + 1900)) == d  -- NOTE: We fix the year so we don't run out of tests
    testNextDoW dow (Positive n) = (dayOfWeek . next n dow $ epochDay) == dow
    testDirection dir adjust (Positive n) = dir n (dayOfWeek epochDay) epochDay == modify (adjust $ n * 7) day epochDay
    epochDay = mkcd 1 March 2000

constructorUnits :: TestTree
constructorUnits = testGroup "Constructor"
  [
     testCase "CalendarDate 30 February 2000 is not a valid date" $ calendarDate 30 February 2000 @?= Nothing
    ,testCase "Gregorian.fromWeekDate 1 Sunday 2000 = 26.Dec.1999" $ G.fromWeekDate 1 Sunday 2000 @?= calendarDate 26 December 1999
    ,testCase "Gregorian.fromWeekDate 5 Sunday 2000 = 23.Jan.2000" $ G.fromWeekDate 5 Sunday 2000 @?= calendarDate 23 January 2000
    ,testCase "Iso.fromWeekDate 1 Sunday 2000 = 9.Jan.2000" $ Iso.fromWeekDate 1 Sunday 2000 @?= calendarDate 9 January 2000
    ,testCase "Iso.fromWeekDate 5 Sunday 2000 = 6.Feb.2000" $ Iso.fromWeekDate 5 Sunday 2000 @?= calendarDate 6 February 2000
  ]

lensUnits :: TestTree
lensUnits = testGroup "Lens"
  [
     testCase "31-January-2000 + 2M == 31-March-2000" $ modify (+2) monthl <$> janEnd @?= calendarDate 31 March 2000
    ,testCase "31-January-2000 + 1M == 29-February-2000" $ modify (+1) monthl <$> janEnd @?= calendarDate 29 February 2000
    ,testCase "29-February-2000 + 1Y == 28-February-2001" $ modify (+1) year <$> leapFeb @?= calendarDate 28 February 2001
    ,testCase "31-December-2000 + 1D == 1-January-2001" $ modify (+1) day <$> endYear @?= calendarDate 1 January 2001
  ]
    where
      janEnd = calendarDate 31 January 2000
      leapFeb = calendarDate 29 February 2000
      endYear = calendarDate 31 December 2000