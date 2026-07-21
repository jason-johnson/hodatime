{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Julian
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Julian' calendar.  The calendar is proleptic in that, while it does start in 45 BC it does not try to account for the fact
-- that before around 4 AD the leap year rule was accidentally implemented as a leap year every three years.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Julian
(
  -- * Constructors
  -- calendarDate
  --,fromNthDay
  --,fromWeekDate
  -- * Types
   Month(..)
  ,DayOfWeek(..)
  ,Julian
  ,yearMonthDayToDays
)
where

import Data.HodaTime.CalendarDateTime.Internal (IsCalendar(..), IsCalendarDateTime(..), DayOfMonth, Year, CalendarDateTime(..), LocalTime(..), Date)
import Data.HodaTime.Instant.Internal (Instant(..))
import Data.HodaTime.Calendar.Internal (mkCommonDayLens, mkCommonMonthLens, mkYearLens, moveByDow, dayOfWeekFromDays, commonMonthDayOffsets, borders, daysPerStandardYear, daysPerFourYears)
import Data.Int (Int32)
import Data.Word (Word8, Word32)
import Control.Arrow ((>>>), (***), (&&&))
import Data.Maybe (fromJust)
import Data.List (findIndex)

-- constants

invalidDayThresh :: Integral a => a
invalidDayThresh = -152445      -- NOTE: 14.Oct.1582, one day before Gregorian calendar came into effect

firstJulDayTuple :: (Integral a, Integral b, Integral c) => (a, b, c)
firstJulDayTuple = (1582, 9, 15)
 
epochDayOfWeek :: DayOfWeek Julian
epochDayOfWeek = Wednesday

-- In case we ever decide to generate a 28 year table to store cycles
-- daysPerSolarCycle :: Num a => a
-- daysPerSolarCycle = 10227

-- types
    
data Julian
    
instance IsCalendar Julian where
  data Date Julian = JulianDate {-# UNPACK #-} !Int32 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word32
    deriving (Eq, Show, Ord)

  data DayOfWeek Julian = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  data Month Julian = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  fromDays = julianFromDays
  toDays = julianToDays
  toYmd = julianToYmd

  day' = mkCommonDayLens invalidDayThresh yearMonthDayToDays julianFromDays julianToYmd
  {-# INLINE day' #-}

  month' (JulianDate _ _ m _) = toEnum . fromIntegral $ m

  monthl' = mkCommonMonthLens firstJulDayTuple maxDaysInMonth yearMonthDayToDays julianToYmd julianFromDays
  {-# INLINE monthl' #-}

  year' = mkYearLens firstJulDayTuple maxDaysInMonth yearMonthDayToDays julianToYmd julianFromDays
  {-# INLINE year' #-}

  dayOfWeek' (JulianDate days _ _ _) = toEnum . dayOfWeekFromDays epochDayOfWeek . fromIntegral $ days

  next' n dow (JulianDate days _ _ _) = moveByDow julianFromDays epochDayOfWeek n dow (-) (+) (>) (fromIntegral days)

  previous' n dow (JulianDate days _ _ _) = moveByDow julianFromDays epochDayOfWeek n dow subtract (-) (<) (fromIntegral days)  -- NOTE: subtract is (-) with the arguments flipped

instance IsCalendarDateTime Julian where
  fromAdjustedInstant (Instant days secs nsecs) = CalendarDateTime (julianFromDays days) (LocalTime secs nsecs)
  toUnadjustedInstant (CalendarDateTime jd (LocalTime secs nsecs)) = Instant (julianToDays jd) secs nsecs

-- | Build the flat Julian date (denormalized: keeps the day count plus the decoded day\/month\/year).
julianFromDays :: Int32 -> Date Julian
julianFromDays days = JulianDate days d m y
  where (y, m, d) = daysToYearMonthDay days

julianToDays :: Date Julian -> Int32
julianToDays (JulianDate days _ _ _) = days

julianToYmd :: Date Julian -> (Word32, Word8, Word8)
julianToYmd (JulianDate _ d m y) = (y, m, d)

-- helper functions

maxDaysInMonth :: Month Julian -> Year -> Int
maxDaysInMonth February y
  | isLeap                                = 29
  | otherwise                             = 28
  where
    isLeap                                = 0 == y `mod` 4
maxDaysInMonth m _
  | m == April || m == June || m == September || m == November  = 30
  | otherwise                                                   = 31

yearMonthDayToDays :: Year -> Month Julian -> DayOfMonth -> Int
yearMonthDayToDays y m d = days
  where
    m' = if m > February then fromEnum m - 2 else fromEnum m + 10
    years = if m < March then y - 2001 else y - 2000
    yearDays = years * daysPerStandardYear + years `div` 4
    days = yearDays + commonMonthDayOffsets !! m' + d - 1

daysToYearMonthDay :: Int32 -> (Word32, Word8, Word8)
daysToYearMonthDay days = (fromIntegral y, fromIntegral m'', fromIntegral d')
  where
    (fourYears, (remaining, isLeapDay)) = flip divMod daysPerFourYears >>> (* 4) *** id &&& borders daysPerFourYears $ days
    (oneYears, yearDays) = remaining `divMod` daysPerStandardYear
    m = pred . fromJust . findIndex (\mo -> yearDays < mo) $ commonMonthDayOffsets
    (m', startDate) = if m >= 10 then (m - 10, 2001) else (m + 2, 2000)
    d = yearDays - commonMonthDayOffsets !! m + 1
    (m'', d') = if isLeapDay then (1, 29) else (m', d)
    y = startDate + fourYears + oneYears