{-# LANGUAGE FlexibleInstances #-}
module HodaTime.Util
(
   RandomOffset(..)
  ,RandomTime(..)
  ,CycleYear(..)
  ,RandomStandardDate(..)
  ,RandomJulianDate(..)
  ,RandomCopticDate(..)
  ,RandomPersianDate(..)
  ,RandomIslamicDate(..)
  ,RandomHebrewDate(..)
)
where

import Test.Tasty.QuickCheck (Arbitrary(..), choose, elements)

import Data.HodaTime.Calendar.Gregorian (Month(..), DayOfWeek(..), Gregorian)
import qualified Data.HodaTime.Calendar.Julian as J
import qualified Data.HodaTime.Calendar.Coptic as C
import qualified Data.HodaTime.Calendar.Persian as P
import qualified Data.HodaTime.Calendar.Islamic as I
import qualified Data.HodaTime.Calendar.Hebrew as H

-- arbitrary data and instances

instance Arbitrary (Month Gregorian) where
  arbitrary = do
    x <- choose (0,11)
    return $ toEnum x

instance Arbitrary (DayOfWeek Gregorian) where
  arbitrary = do
    x <- choose (0,6)
    return $ toEnum x

data RandomOffset = RandomOffset Int Int Int
  deriving (Show)

instance Arbitrary RandomOffset where
  arbitrary = do
    h <- choose (-17,17)
    m <- choose (0,59)
    s <- choose (0,59)
    which <- choose (1,100)
    return (chosen which $ RandomOffset h m s)
      where
        chosen :: Int -> RandomOffset -> RandomOffset
        chosen n ro
          | n > 98     = RandomOffset 18 0 0
          | n > 96    = RandomOffset (-18) 0 0
          | otherwise = ro

data RandomTime = RandomTime Int Int Int
  deriving (Show)

instance Arbitrary RandomTime where
  arbitrary = do
    h <- choose (0,23)
    m <- choose (0,59)
    s <- choose (0,59)
    return $ RandomTime h m s

newtype CycleYear = CycleYear Int
  deriving (Show)

instance Arbitrary CycleYear where
  arbitrary = do
    y <- choose (0,399)
    return $ CycleYear y

data RandomStandardDate = RandomStandardDate Int (Month Gregorian) Int
  deriving (Show)

instance Arbitrary RandomStandardDate where
  arbitrary = do
    y <- choose (1972,2040)
    m <- choose (0,11)
    d <- choose (1,28)
    return $ RandomStandardDate y (toEnum m) d

instance Arbitrary (J.Month J.Julian) where
  arbitrary = do
    x <- choose (0,11)
    return $ toEnum x

instance Arbitrary (J.DayOfWeek J.Julian) where
  arbitrary = do
    x <- choose (0,6)
    return $ toEnum x

-- | A random valid Julian date.  The Julian calendar runs from its introduction on 1.Jan.45 BC (astronomical year
--   -44) with no upper bound, so the range spans 45 BC through the modern era and exercises BC (negative) years.  The
--   day is capped at 28 so every generated (year, month, day) is a real date.
data RandomJulianDate = RandomJulianDate Int (J.Month J.Julian) Int
  deriving (Show)

instance Arbitrary RandomJulianDate where
  arbitrary = do
    y <- choose (-44,2400)
    m <- choose (0,11)
    d <- choose (1,28)
    return $ RandomJulianDate y (toEnum m) d

instance Arbitrary (C.Month C.Coptic) where
  arbitrary = do
    x <- choose (0,12)
    return $ toEnum x

instance Arbitrary (C.DayOfWeek C.Coptic) where
  arbitrary = do
    x <- choose (0,6)
    return $ toEnum x

-- | A random valid Coptic date.  Months 1\-12 have 30 days; the thirteenth month (the epagomenal days) has only 5 (6
--   in a leap year), so we cap its day at 5 to keep every generated (year, month, day) valid regardless of leap year.
data RandomCopticDate = RandomCopticDate Int (C.Month C.Coptic) Int
  deriving (Show)

instance Arbitrary RandomCopticDate where
  arbitrary = do
    y <- choose (1,2000)
    m <- choose (0,12)
    d <- if m == 12 then choose (1,5) else choose (1,30)
    return $ RandomCopticDate y (toEnum m) d

instance Arbitrary (P.Month P.Persian) where
  arbitrary = do
    x <- choose (0,11)
    return $ toEnum x

instance Arbitrary (P.DayOfWeek P.Persian) where
  arbitrary = do
    x <- choose (0,6)
    return $ toEnum x

-- | A random valid Persian date.  Months 1\-6 have 31 days, months 7\-11 have 30, and 'Esfand' has 29 (30 in a leap
--   year), so we cap 'Esfand' at 29 to keep every generated (year, month, day) valid regardless of leap year.  The year
--   is kept within the astronomical calendar's supported range.
data RandomPersianDate = RandomPersianDate Int (P.Month P.Persian) Int
  deriving (Show)

instance Arbitrary RandomPersianDate where
  arbitrary = do
    y <- choose (1,1500)
    m <- choose (0,11)
    d <- if m < 6 then choose (1,31) else if m < 11 then choose (1,30) else choose (1,29)
    return $ RandomPersianDate y (toEnum m) d

instance Arbitrary (I.Month I.IslamicBcl) where
  arbitrary = do
    x <- choose (0,11)
    return $ toEnum x

instance Arbitrary (I.DayOfWeek I.IslamicBcl) where
  arbitrary = do
    x <- choose (0,6)
    return $ toEnum x

-- | A random valid Islamic date.  Odd-numbered months have 30 days and even-numbered months 29 ('DhulHijjah' gains a
--   30th only in a leap year), so we cap the even months (including 'DhulHijjah') at 29 to keep every generated
--   (year, month, day) valid regardless of leap year.
data RandomIslamicDate = RandomIslamicDate Int (I.Month I.IslamicBcl) Int
  deriving (Show)

instance Arbitrary RandomIslamicDate where
  arbitrary = do
    y <- choose (1,2000)
    m <- choose (0,11)
    d <- if even m then choose (1,30) else choose (1,29)
    return $ RandomIslamicDate y (toEnum m) d

instance Arbitrary (H.Month H.HebrewCivil) where
  arbitrary = do
    x <- choose (0,12)
    return $ toEnum x

instance Arbitrary (H.DayOfWeek H.HebrewCivil) where
  arbitrary = do
    x <- choose (0,6)
    return $ toEnum x

-- | The Hebrew months in calendar order, each paired with a day it is always safe to generate.  'Cheshvan' and
--   'Kislev' are the two swing months (29 or 30 days depending on the year), so they are capped at their shorter length
--   of 29; every other length is fixed.  'AdarI' (the leap month) is only ever included for leap years (see below).
hebrewMonthCaps :: [(H.Month H.HebrewCivil, Int)]
hebrewMonthCaps =
  [ (H.Tishri, 30), (H.Cheshvan, 29), (H.Kislev, 29), (H.Tevet, 29), (H.Shevat, 30)
  , (H.AdarI, 30), (H.Adar, 29), (H.Nisan, 30), (H.Iyar, 29), (H.Sivan, 30)
  , (H.Tammuz, 29), (H.Av, 30), (H.Elul, 29) ]

-- | A random valid Hebrew (civil) date.  A common year has no leap month, so 'AdarI' is offered only in leap years
--   (Metonic years 3, 6, 8, 11, 14, 17, 19); the swing months 'Cheshvan' and 'Kislev' are capped at their shorter
--   length so every generated (year, month, day) is a real date regardless of the year's exact length.
data RandomHebrewDate = RandomHebrewDate Int (H.Month H.HebrewCivil) Int
  deriving (Show)

instance Arbitrary RandomHebrewDate where
  arbitrary = do
    y <- choose (1,6000)
    let months = if (7 * y + 1) `mod` 19 < 7 then hebrewMonthCaps else filter ((/= H.AdarI) . fst) hebrewMonthCaps
    (m, cap) <- elements months
    d <- choose (1,cap)
    return $ RandomHebrewDate y m d