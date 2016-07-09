{-# LANGUAGE TypeFamilies #-}

module Data.HodaTime.Calendar.Internal
(
   IslamicLeapYearPattern(..)
  ,IslamicEpoch(..)
  ,HebrewMonthNumbering(..)
  ,Calendar(..)
  ,LocalDate(..)
  ,IsCalendar(..)
  ,HasDate(..)
)
where

import Data.Int (Int8, Int16)
import Data.Ord (comparing)
import Data.Monoid ((<>))

data IslamicLeapYearPattern =
    ILYPBase15
  | ILYPBase16
  | ILYPIndian
  | ILYPHabashAlHasib
    deriving (Eq, Show)

data IslamicEpoch =
    IslamicAstronomical
  | IslamicCivil
    deriving (Eq, Show)

data HebrewMonthNumbering =
    HebrewCivil
  | HebrewScriptural
    deriving (Eq, Show)

data Calendar =
    Iso
  | Gregorian           -- TODO: Add min week day thing?
  | Persian
  | Hebrew HebrewMonthNumbering
  | Coptic              -- TODO: Add min week day thing?
  | Islamic IslamicLeapYearPattern IslamicEpoch
    deriving (Eq, Show)

data CalDateTime m calendar = CalDateTime (LocalDate m calendar) Int Int Int

-- | Represents a specific date within its calendar system, with no reference to any time zone or time of day.
data LocalDate m calendar = LocalDate { ldYear :: Int16, ldMonth :: m, ldDay :: Int8 }
    deriving (Eq, Show)

instance Ord m => Ord (LocalDate m c) where
  compare a b = comparing ldYear a b <> comparing ldMonth a b <> comparing ldDay a b

class IsCalendar cal where
  type CalendarDate cal
  data DayOfWeek cal
  data Month cal
  next' :: DayOfWeek cal -> CalendarDate cal -> CalendarDate cal

class HasDate dt where
  next :: a -> dt -> dt

instance IsCalendar cal => HasDate (LocalDate m cal) where
  next = undefined

instance IsCalendar cal => HasDate (CalDateTime m cal) where
  next i (CalDateTime cd h m s) = CalDateTime (next i cd) h m s
