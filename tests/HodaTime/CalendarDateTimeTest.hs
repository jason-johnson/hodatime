{-# LANGUAGE FlexibleInstances #-}

module HodaTime.CalendarDateTimeTest
(
  calendarDateTimeTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Maybe (fromJust)

import HodaTime.Util
import Data.HodaTime.LocalTime (localTime, HasLocalTime(..))
import Data.HodaTime.CalendarDate (day, monthl, month, year, next, previous, dayOfWeek)
import Data.HodaTime.Calendar.Gregorian (calendarDate, Month(..), DayOfWeek(..), Gregorian)
import Data.HodaTime.CalendarDateTime (on, at)

calendarDateTimeTests :: TestTree
calendarDateTimeTests = testGroup "CalendarDateTimeTests Tests" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [timeLensProps, dateLensProps]

timeLensProps :: TestTree
timeLensProps = testGroup "Time Lens"
  [
     QC.testProperty "get seconds offset" $ testGet second _1
    ,QC.testProperty "get minutes offset" $ testGet minute _2
    ,QC.testProperty "get hours offset" $ testGet hour _3
    ,QC.testProperty "modify seconds offset" $ testF (modify . (+)) second _1 (+) 5
    ,QC.testProperty "modify minutes offset" $ testF (modify . (+)) minute _2 (+) 5
    ,QC.testProperty "modify hours offset" $ testF (modify . (+)) hour _3 (+) 5
    ,QC.testProperty "set seconds offset" $ testF set second _1 const 5
    ,QC.testProperty "set minutes offset" $ testF set minute _2 const 5
    ,QC.testProperty "set hours offset" $ testF set hour _3 const 5
  ]
  where
    mkTime h m s = fromJust $ on <$> localTime h m s 0 <*> calendarDate 1 April 2001   -- We are already controlling that only valid values will be passed in
    offsetEq (s, m, h) off = get second off == s && get minute off == m && get hour off == h
    _1 f (a,b,c) = (\a' -> (a',b,c)) <$> f a
    _2 f (a,b,c) = (\b' -> (a,b',c)) <$> f b
    _3 f (a,b,c) = (\c' -> (a,b,c')) <$> f c
    testGet l l' (Positive s, Positive m, Positive h) = h < 23 && s < 60 && m < 60 QC.==> get l (mkTime h m s) == get l' (s, m, h)
    testF f l l' g n (Positive s, Positive m, Positive h) = h < 23 - n && s < 60 - n && m < 60 - n QC.==> offsetEq (modify (g n) l' (s,m,h)) $ f n l (mkTime h m s)

instance Arbitrary (Month Gregorian) where
  arbitrary = do
    x <- choose (0,11)
    return $ toEnum x

instance Arbitrary (DayOfWeek Gregorian) where
  arbitrary = do
    x <- choose (0,6)
    return $ toEnum x

dateLensProps :: TestTree
dateLensProps = testGroup "Date Lens"
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