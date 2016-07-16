{-# LANGUAGE TypeFamilies #-}

module Data.HodaTime.Calendar.Internal
(
   IslamicLeapYearPattern(..)
  ,IslamicEpoch(..)
  ,HebrewMonthNumbering(..)
  ,Calendar(..)
  ,DayNth(..)
  ,CalendarDate(..)
  ,CalendarDateTime(..)
  ,IsCalendar(..)
  ,HasDate(..)
)
where

import Data.HodaTime.LocalTime.Internal (LocalTime(..))
import Data.Int (Int32)

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

data DayNth =
    First
  | Second
  | Third
  | Fourth
  | Fifth
  | Last
  | SecondToLast
  | ThirdToLast
  | FourthToLast
    deriving (Eq, Show)

data CalendarDateTime o calendar = CalendarDateTime (CalendarDate o calendar) LocalTime
  deriving (Eq, Show, Ord)

-- | Represents a specific date within its calendar system, with no reference to any time zone or time of day.
data CalendarDate o calendar = CalendarDate { cdDays :: Int32, ldOptions :: o }
  deriving (Eq, Show, Ord)

class IsCalendar cal where
  type Date cal
  data DayOfWeek cal
  data Month cal
  type CalendarOptions cal
  next' :: Int -> DayOfWeek cal -> CalendarDate (CalendarOptions cal) cal -> CalendarDate (CalendarOptions cal) cal
  previous' :: Int -> DayOfWeek cal -> CalendarDate (CalendarOptions cal) cal -> CalendarDate (CalendarOptions cal) cal

class HasDate d where
  type DoW d
  next :: Int -> DoW d -> d -> d
  previous :: Int -> DoW d -> d -> d

instance (IsCalendar cal, o ~ CalendarOptions cal) => HasDate (CalendarDate o cal) where
  type DoW (CalendarDate o cal) = DayOfWeek cal
  next = next'
  previous = previous'

instance (IsCalendar cal, o ~ CalendarOptions cal) => HasDate (CalendarDateTime o cal) where
  type DoW (CalendarDateTime o cal) = DayOfWeek cal
  next i dow (CalendarDateTime cd lt) = CalendarDateTime (next i dow cd) lt
  previous i dow (CalendarDateTime cd lt) = CalendarDateTime (previous i dow cd) lt
