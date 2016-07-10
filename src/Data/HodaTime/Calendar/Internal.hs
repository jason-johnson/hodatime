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

data CalendarDateTime m o calendar = CalendarDateTime (CalendarDate m o calendar) Int Int Int
  deriving (Eq, Show)

-- | Represents a specific date within its calendar system, with no reference to any time zone or time of day.
data CalendarDate m o calendar = CalendarDate { ldYear :: Int16, ldMonth :: m, ldDay :: Int8, ldOptions :: o }
  deriving (Eq, Show)

instance (Ord m, Eq o) => Ord (CalendarDate m o c) where
  compare a b = comparing ldYear a b <> comparing ldMonth a b <> comparing ldDay a b

class IsCalendar cal where
  type Date cal
  data DayOfWeek cal
  data Month cal
  type CalendarOptions cal
  next' :: DayOfWeek cal -> CalendarDate (Month cal) (CalendarOptions cal) cal -> CalendarDate (Month cal) (CalendarOptions cal) cal
  previous' :: DayOfWeek cal -> CalendarDate (Month cal) (CalendarOptions cal) cal -> CalendarDate (Month cal) (CalendarOptions cal) cal

class HasDate d where
  type DoW d
  next :: DoW d -> d -> d
  previous :: DoW d -> d -> d

instance (IsCalendar cal, m ~ Month cal, o ~ CalendarOptions cal) => HasDate (CalendarDate m o cal) where
  type DoW (CalendarDate m o cal) = DayOfWeek cal
  next = next'
  previous = previous'

instance (IsCalendar cal, m ~ Month cal, o ~ CalendarOptions cal) => HasDate (CalendarDateTime m o cal) where
  type DoW (CalendarDateTime m o cal) = DayOfWeek cal
  next dow (CalendarDateTime cd h m s) = CalendarDateTime (next dow cd) h m s
  previous dow (CalendarDateTime cd h m s) = CalendarDateTime (previous dow cd) h m s
