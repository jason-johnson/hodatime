{-# LANGUAGE TypeFamilies #-}

module Data.HodaTime.Calendar.Internal
(
   IslamicLeapYearPattern(..)
  ,IslamicEpoch(..)
  ,HebrewMonthNumbering(..)
  ,Calendar(..)
  ,CalendarDate(..)
  ,CalendarDateTime(..)
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

data CalendarDateTime m calendar = CalendarDateTime (CalendarDate m calendar) Int Int Int
  deriving (Eq, Show)

-- | Represents a specific date within its calendar system, with no reference to any time zone or time of day.
data CalendarDate m calendar = CalendarDate { ldYear :: Int16, ldMonth :: m, ldDay :: Int8 }
  deriving (Eq, Show)

instance Ord m => Ord (CalendarDate m c) where
  compare a b = comparing ldYear a b <> comparing ldMonth a b <> comparing ldDay a b

class IsCalendar cal where
  type Date cal
  data DayOfWeek cal
  data Month cal
  next' :: DayOfWeek cal -> CalendarDate (Month cal) cal -> CalendarDate (Month cal) cal

class HasDate d where
  type Input d
  next :: Input d -> d -> d

instance (IsCalendar cal, m ~ Month cal) => HasDate (CalendarDate m cal) where
  type Input (CalendarDate m cal) = DayOfWeek cal
  next = next'

instance (IsCalendar cal, m ~ Month cal) => HasDate (CalendarDateTime m cal) where
  type Input (CalendarDateTime m cal) = DayOfWeek cal
  next dow (CalendarDateTime cd h m s) = CalendarDateTime (next dow cd) h m s
