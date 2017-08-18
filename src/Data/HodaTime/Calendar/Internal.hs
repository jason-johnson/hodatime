{-# LANGUAGE TypeFamilies #-}

module Data.HodaTime.Calendar.Internal
(
   DayNth(..)
  ,CalendarDate(..)
  ,CalendarDateTime(..)
  ,IsCalendar(..)
  ,HasDate(..)
)
where

import Data.HodaTime.LocalTime.Internal (LocalTime(..))
import Data.Int (Int32)
import Data.Word (Word8, Word32)

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
    deriving (Eq, Show, Enum)

data CalendarDateTime o calendar = CalendarDateTime (CalendarDate o calendar) LocalTime
  deriving (Eq, Show, Ord)

-- | Represents a specific date within its calendar system, with no reference to any time zone or time of day.
-- Note: We keep the date in 2 formats, redundantly.  We depend on lazy evaluation to only produce the portion that is actually used
--data CalendarDate o calendar = CalendarDate { cdDays :: Int32, cdDay :: Word8, cdMonth :: Word8, cdYear :: Word32, ldOptions :: o }
data CalendarDate o calendar = CalendarDate { cdDays :: Int32, ldOptions :: o }
  deriving (Eq, Show, Ord)

-- NOTE: This is a test form of the calendar date that only stores the cycle.  Everything else will be pulled from the date cache table, as required
--data CalendarDate o calendar = CalendarDate { cdDays :: Int32, cdCycle :: Word8, ldOptions :: o }
--  deriving (Eq, Show, Ord)

class IsCalendar cal where
  type Date cal
  data DayOfWeek cal
  data Month cal
  type CalendarOptions cal
  dayOfWeek' :: CalendarDate (CalendarOptions cal) cal -> DayOfWeek cal
  next' :: Int -> DayOfWeek cal -> CalendarDate (CalendarOptions cal) cal -> CalendarDate (CalendarOptions cal) cal
  previous' :: Int -> DayOfWeek cal -> CalendarDate (CalendarOptions cal) cal -> CalendarDate (CalendarOptions cal) cal

class HasDate d where
  type DoW d
  dayOfWeek :: d -> DoW d
  next :: Int -> DoW d -> d -> d
  previous :: Int -> DoW d -> d -> d

instance (IsCalendar cal, o ~ CalendarOptions cal) => HasDate (CalendarDate o cal) where
  type DoW (CalendarDate o cal) = DayOfWeek cal
  dayOfWeek = dayOfWeek'
  next = next'
  previous = previous'

instance (IsCalendar cal, o ~ CalendarOptions cal) => HasDate (CalendarDateTime o cal) where
  type DoW (CalendarDateTime o cal) = DayOfWeek cal
  dayOfWeek (CalendarDateTime cd _) = dayOfWeek cd
  next i dow (CalendarDateTime cd lt) = CalendarDateTime (next i dow cd) lt
  previous i dow (CalendarDateTime cd lt) = CalendarDateTime (previous i dow cd) lt
