{-# LANGUAGE TypeFamilies #-}

module Data.HodaTime.Calendar.Gregorian.Internal
(
   daysToYearMonthDay
  ,fromWeekDate
  ,Gregorian
  ,Month(..)
  ,DayOfWeek(..)
  ,invalidDayThresh
  ,epochDayOfWeek
  ,maxDaysInMonth
  ,yearMonthDayToDays
  ,nthDayToDayOfMonth
  ,dayOfWeekFromDays
  ,instantToYearMonthDay
)
where

import Data.HodaTime.Constants (daysPerCycle, daysPerCentury)
import Data.HodaTime.CalendarDateTime.Internal (IsCalendar(..), CalendarDate(..), IsCalendarDateTime(..), DayOfMonth, Year, WeekNumber, CalendarDateTime(..), LocalTime(..))
import Data.HodaTime.Calendar.Gregorian.CacheTable (DTCacheTable(..), decodeMonth, decodeYear, decodeDay, cacheTable)
import Data.HodaTime.Calendar.Constants (daysPerStandardYear)
import Data.HodaTime.Instant.Internal (Instant(..))
import Control.Arrow ((>>>), (&&&), (***), first)
import Data.Int (Int32)
import Data.Word (Word8, Word32)
import Data.Array.Unboxed ((!))
import Control.Monad (guard)

-- Constants

invalidDayThresh :: Integral a => a
invalidDayThresh = -152445      -- NOTE: 14.Oct.1582, one day before Gregorian calendar came into effect

firstGregDayTuple :: (Integral a, Integral b, Integral c) => (a, b, c)
firstGregDayTuple = (1582, 9, 15)
    
epochDayOfWeek :: DayOfWeek Gregorian
epochDayOfWeek = Wednesday

monthDayOffsets :: Num a => [a]
monthDayOffsets = 0 : rest
  where
    rest = zipWith (+) daysPerMonth (0:rest)
    daysPerMonth = [31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 28]         -- NOTE: rotated (TODO BUG: Why do we need Feb?  That will be past end of year, thus impossible)
    
-- types
    
data Gregorian
    
instance IsCalendar Gregorian where
  type Date Gregorian = CalendarDate Gregorian
    
  data DayOfWeek Gregorian = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
    
  data Month Gregorian = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
    
  day' f (CalendarDate _ d m y) = mkcd . (rest+) <$> f (fromIntegral d)
    where
      rest = pred $ yearMonthDayToDays (fromIntegral y) (toEnum . fromIntegral $ m) 1
      mkcd days =
        let
          days' = fromIntegral $ if days > invalidDayThresh then days else invalidDayThresh + 1
          (y', m', d') = daysToYearMonthDay days'
        in CalendarDate days' d' m' y'
  {-# INLINE day' #-}
    
  month' (CalendarDate _ _ m _) = toEnum . fromIntegral $ m
    
  monthl' f (CalendarDate _ d m y) = mkcd <$> f (fromEnum m)
    where
      mkcd months = CalendarDate (fromIntegral days) d'' (fromIntegral m') (fromIntegral y'')
        where
          (y', months') = flip divMod 12 >>> first (+ fromIntegral y) $ months
          (y'', m', d') = if (y', months', d) < firstGregDayTuple then firstGregDayTuple else (y', months', d)
          mdim = fromIntegral $ maxDaysInMonth (toEnum m') y'
          d'' = if d' > mdim then mdim else d'
          days = yearMonthDayToDays y'' (toEnum m') (fromIntegral d'')
  {-# INLINE monthl' #-}
    
  year' f (CalendarDate _ d m y) = mkcd <$> f (fromIntegral y)
    where
      mkcd y' = CalendarDate days d'' m' (fromIntegral y'')
        where
          (y'', m', d') = if (y', m, d) < firstGregDayTuple then firstGregDayTuple else (y', m, d)
          m'' = toEnum . fromIntegral $ m'
          mdim = fromIntegral $ maxDaysInMonth m'' y''
          d'' = if d' > mdim then mdim else d'
          days = fromIntegral $ yearMonthDayToDays y'' m'' (fromIntegral d'')
  {-# INLINE year' #-}
    
  dayOfWeek' (CalendarDate days _ _ _) = toEnum . dayOfWeekFromDays . fromIntegral $ days
    
  next' n dow (CalendarDate days _ _ _) = moveByDow n dow (-) (+) (fromIntegral days)
    
  previous' n dow (CalendarDate days _ _ _) = moveByDow n dow subtract (-) (fromIntegral days)  -- NOTE: subtract is (-) with the arguments flipped

instance IsCalendarDateTime Gregorian where
  fromAdjustedInstant (Instant days secs nsecs) = CalendarDateTime cd lt
    where
      cd = CalendarDate days d m y
      (y, m, d) = daysToYearMonthDay days
      lt = LocalTime secs nsecs

  toUnadjustedInstant (CalendarDateTime (CalendarDate days _ _ _) (LocalTime secs nsecs)) = Instant days secs nsecs

-- constructors

fromWeekDate :: Int -> DayOfWeek Gregorian -> WeekNumber -> DayOfWeek Gregorian -> Year -> Maybe (Date Gregorian)
fromWeekDate minWeekDays wkStartDoW weekNum dow y = do
  guard $ days > invalidDayThresh
  return $ CalendarDate days d m y'
    where
      soyDays = yearMonthDayToDays y January minWeekDays
      soyDoW = dayOfWeekFromDays soyDays
      startDoWDistance = fromEnum soyDoW - fromEnum wkStartDoW
      dowDistance = fromEnum dow - fromEnum wkStartDoW
      dowDistance' = if dowDistance < 0 then dowDistance + 7 else dowDistance
      startDays = soyDays - startDoWDistance
      weekNum' = pred weekNum
      days = fromIntegral $ startDays + weekNum' * 7 + dowDistance'
      (y', m, d) = daysToYearMonthDay days

-- helper functions

nthDayToDayOfMonth :: Int -> Int -> Month Gregorian -> Int -> Int
nthDayToDayOfMonth nth day month y = dom + d' + 7 * nth
  where
    mdm = maxDaysInMonth month y
    dom = if nth < 0 then mdm else 1
    m = fromEnum month
    dow = (dom + (13 * m' - 1) `div` 5 + yrhs + (yrhs `div` 4) + (ylhs `div` 4) - 2 * ylhs) `mod` 7
    d = day - dow
    d' = if d < 0 then d + 7 else d
    (m', y') = if m < 2 then (m + 11, y - 1) else (m - 1, y)
    yrhs = y' `mod` 100
    ylhs = y' `div` 100

dayOfWeekFromDays :: Int -> Int
dayOfWeekFromDays = normalize . (fromEnum epochDayOfWeek +) . flip mod 7
  where
    normalize n = if n >= 7 then n - 7 else n

moveByDow :: Int -> DayOfWeek Gregorian -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> Int -> CalendarDate Gregorian
moveByDow n dow distanceF adjust days = CalendarDate days' d m y
  where
    currentDoW = dayOfWeekFromDays days
    targetDow = fromIntegral . fromEnum $ dow
    distance = distanceF targetDow currentDoW
    days' = fromIntegral $ fromIntegral days `adjust` (7 * n) `adjust` distance
    (y, m, d) = daysToYearMonthDay days'

maxDaysInMonth :: Month Gregorian -> Year -> Int
maxDaysInMonth February y
  | isLeap                                = 29
  | otherwise                             = 28
  where
    isLeap
      | 0 == y `mod` 100                  = 0 == y `mod` 400
      | otherwise                         = 0 == y `mod` 4
maxDaysInMonth m _
  | m == April || m == June || m == September || m == November  = 30
  | otherwise                                                   = 31

-- NOTE: Epoch is March 1 2000 because that has nicest properties that is near our current time.
-- TODO: The addition of leap days below will add from the previous year.  We need to determine if this is a bug
-- TODO: and if it is not, why isn't it
yearMonthDayToDays :: Year -> Month Gregorian -> DayOfMonth -> Int
yearMonthDayToDays y m d = days
  where
    m' = if m > February then fromEnum m - 2 else fromEnum m + 10
    years = if m < March then y - 2001 else y - 2000
    yearDays = years * daysPerStandardYear + years `div` 4 + years `div` 400 - years `div` 100
    days = yearDays + monthDayOffsets !! m' + d - 1

-- | The issue is that 4 * daysPerCentury will be one less than daysPerCycle.  The reason for this is that the Gregorian calendar adds one more day per 400 year cycle
--   and this day is missing from adding up 4 individual centuries.  We have the same issue again with 4 years (i.e. 365*4 is daysPerFourYears - 1)
--   so we use this function to check if this has occurred so we can add the missing day back in.
borders :: (Num a, Eq a) => a -> a -> Bool
borders c x = x == c - 1
  
-- | Count up centuries, plus remaining days and determine if this is a special extra cycle day.  NOTE: This
--   function would be more accurate if it only took absolute values, but it does end up coming up with the correct answer even on negatives.  It just
--   ends up doing extra calculations with negatives (e.g. year comes back as -100 and entry is +100, which ends up being right but it could have been 0 and the +0 entry)
calculateCenturyDays :: Int32 -> (Int32, Int32, Bool)
calculateCenturyDays days = (y, centuryDays, isExtraCycleDay)
  where
    (cycleYears, (cycleDays, isExtraCycleDay)) = flip divMod daysPerCycle >>> (* 400) *** id &&& borders daysPerCycle $ days
    (centuryYears, centuryDays) = flip divMod daysPerCentury >>> first (* 100) $ cycleDays
    y = cycleYears + centuryYears

daysToYearMonthDay :: Int32 -> (Word32, Word8, Word8)
daysToYearMonthDay days = (fromIntegral y', m'', fromIntegral d')
  where
    (centuryYears, centuryDays, isExtraCycleDay) = calculateCenturyDays days
    decodeEntry (DTCacheTable xs _) = (\x -> (decodeYear x, decodeMonth x, decodeDay x)) . (!) xs
    (y,m,d) = decodeEntry cacheTable . fromIntegral $ centuryDays
    (m',d') = if isExtraCycleDay then (1,29) else (m,d)
    (y',m'') = (2000 + centuryYears + fromIntegral y, fromIntegral $ m')

-- here to avoid circular dependancy between Instant and Gregorian
instantToYearMonthDay :: Instant -> (Word32, Word8, Word8)
instantToYearMonthDay (Instant days _ _) = daysToYearMonthDay days