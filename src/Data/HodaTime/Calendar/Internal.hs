{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}

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

data CalDateTime m c = CalDateTime (LocalDate m c) Int Int Int

-- | Represents a specific date within its calendar system, with no reference to any time zone or time of day.
data LocalDate m (c :: Calendar) = LocalDate Int16 m Int8
    deriving (Eq, Show)

class IsCalendar (cal :: Calendar) where
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

instance IsCalendar 'Gregorian where
  type CalendarDate 'Gregorian = LocalDate (Month 'Gregorian) 'Gregorian
  data DayOfWeek 'Gregorian = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Eq, Ord, Enum, Bounded)
  data Month 'Gregorian = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show, Eq, Ord, Enum, Bounded)

  next' = undefined
