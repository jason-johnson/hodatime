{-# LANGUAGE FlexibleInstances #-}
module HodaTime.Util
(
   RandomOffset(..)
  ,RandomTime(..)
  ,CycleYear(..)
  ,RandomStandardDate(..)
  ,RandomJulianDate(..)
  ,get
  ,modify
  ,set
)
where

import Test.Tasty.QuickCheck (Arbitrary(..), choose)

import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..))

import Data.HodaTime.Calendar.Gregorian (Month(..), DayOfWeek(..), Gregorian)
import qualified Data.HodaTime.Calendar.Julian as J

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

-- | A random valid Julian date.  Unlike Gregorian (which rejects pre\-1582 dates), the Julian calendar is valid for
--   all AD years, so the range spans early\-medieval through modern years to exercise the pre\-1582 dates the calendar
--   exists to represent.  The day is capped at 28 so every generated (year, month, day) is a real date.
data RandomJulianDate = RandomJulianDate Int (J.Month J.Julian) Int
  deriving (Show)

instance Arbitrary RandomJulianDate where
  arbitrary = do
    y <- choose (1,2400)
    m <- choose (0,11)
    d <- choose (1,28)
    return $ RandomJulianDate y (toEnum m) d

-- Lenses

get :: ((s -> Const s c) -> a -> Const t b) -> a -> t
get l = getConst . l Const
  
modify :: (s -> b) -> ((s -> Identity b) -> a -> Identity t) -> a -> t
modify f l = runIdentity . l (Identity . f)
  
set :: s -> ((b -> Identity s) -> a -> Identity t) -> a -> t
set v = modify (const v)