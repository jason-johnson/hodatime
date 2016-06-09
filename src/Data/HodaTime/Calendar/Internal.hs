{-# LANGUAGE DataKinds, KindSignatures #-}

module Data.HodaTime.Calendar.Internal
(
   Calendar(..)
  ,gregorianDate
  ,isoDate
)
where

import Data.HodaTime.LocalDateTime.Internal (LocalDate(..), LocalDateTime(..))

-- TODO: This will take more thought.  A LocalDate[Time] only makes sense within a specific calendar system.  This should be encoded in the type somehow.
-- TODO: Perhaps a type family, something like:

data CalDateTime c = CalDateTime (CalDate c) Int Int Int

data CalDate (c :: Calendar) = CalDate Int
  deriving (Eq, Show)

data Calendar =
    Gregorian
  | Iso
  | Hebrew
    deriving (Eq, Show)

gregorianDate :: Int -> CalDate 'Gregorian
gregorianDate = CalDate

isoDate :: Int -> CalDate 'Iso
isoDate = CalDate

class HasCalendar cal where
  next' :: Int -> CalDate cal -> CalDate cal

class HasDate dt where
  next :: Int -> dt -> dt

instance HasCalendar cal => HasDate (CalDate cal) where
  next = next'

instance HasCalendar cal => HasDate (CalDateTime cal) where
  next i (CalDateTime cd h m s) = CalDateTime (next i cd) h m s

instance HasCalendar 'Gregorian where
  next' i dt = dt
