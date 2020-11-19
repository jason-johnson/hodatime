{-# LANGUAGE FlexibleInstances #-}
module HodaTime.Util
(
   RandomOffset(..)
  ,RandomTime(..)
  ,CycleYear(..)
  ,RandomStandardDate(..)
  ,ValidTimeZoneName(..)
  ,get
  ,modify
  ,set
)
where

import Test.Tasty.QuickCheck (Arbitrary(..), Gen, choose)
import Test.QuickCheck.GenT hiding (choose)

import Control.Monad.IO.Class (liftIO)
import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..))

import Data.HodaTime.Instant
import Data.HodaTime.Calendar.Gregorian (Month(..), DayOfWeek(..), Gregorian)
import Data.HodaTime.Offset (Offset, fromSeconds, minOffsetSeconds, maxOffsetSeconds)
import Data.HodaTime.TimeZone

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

data RandomStandardDate = RandomStandardDate Int Int Int
  deriving (Show)

instance Arbitrary RandomStandardDate where
  arbitrary = do
    y <- choose (1972,2040)
    m <- choose (0,11)
    d <- choose (1,28)
    return $ RandomStandardDate y m d

instance Arbitrary Instant where
  arbitrary = fromSecondsSinceUnixEpoch <$> arbitrary

instance Arbitrary Offset where
  arbitrary = do
    s <- choose (minOffsetSeconds, maxOffsetSeconds) :: Gen Int
    return $ fromSeconds s

newtype ValidTimeZoneName = ValidTimeZoneName String deriving (Show)

instance Arbitrary ValidTimeZoneName where
  arbitrary = ValidTimeZoneName <$> elements [
      "UTC"
    , "Europe/Stockholm"
    ]

-- Lenses

get :: ((s -> Const s c) -> a -> Const t b) -> a -> t
get l = getConst . l Const
  
modify :: (s -> b) -> ((s -> Identity b) -> a -> Identity t) -> a -> t
modify f l = runIdentity . l (Identity . f)
  
set :: s -> ((b -> Identity s) -> a -> Identity t) -> a -> t
set v = modify (const v)
