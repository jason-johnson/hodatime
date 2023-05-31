module Data.HodaTime.Calendar.Gregorian.CacheTable
(
   DTCacheTable(..)
  ,cacheTable
  ,decodeYear
  ,decodeMonth
  ,decodeDay
  ,decodeHour
  ,decodeMinute
  ,decodeSecond
)
where

import Data.Word (Word16)
import Data.Bits (shift, (.|.), (.&.), shiftR)
import Data.Array.Unboxed (array, UArray)

type DTCacheDaysTable = UArray Int Word16
type DTCacheHoursTable = UArray Int Word16

data DTCacheTable = DTCacheTable DTCacheDaysTable DTCacheHoursTable

-- TODO: The start date is offset but otherwise the months are all in their
--        unrotated form (i.e. Jan/Feb are in their normal year, not the previous year).
--        does this hurt anything?  It's nice for decoding but maybe some math expects
--        Jan/Feb to be (year - 1)

-- The Cache Table holds years and hours in the following format:
-- +-----+----+----+  +----+----+----+
-- |0-100|1-12|1-31|  |0-11|0-59|0-59|
-- +-----+----+----+  +----+----+----+
--    7     4   5        4    6    6
-- Meaning we can store 100 years of days and 12 hours of seconds in 16 bits each
cacheTable :: DTCacheTable
cacheTable = DTCacheTable days hours where
  toArray xs = array (0, length xs - 1) $ zip [0..] xs
  days = toArray $ firstYear ++ years ++ lastYear
  firstYear = [ encodeDate 0 m d | m <- [2..11], d <- daysInMonth m 0]
  years = [ encodeDate y m d | y <- [1..99], m <- [0..11], d <- daysInMonth m y]
  lastYear = [ encodeDate 100 m d | m <- [0, 1], d <- daysInMonth m 100]
  hours = toArray [ encodeTime h m s | h <- [0..11], m <- [0..59], s <- [0..59]]

-- encode

yearShift :: Num a => a
yearShift = 9

monthShift :: Num a => a
monthShift = 5

encodeDate :: Word16 -> Word16 -> Word16 -> Word16
encodeDate y m d = shift y yearShift .|. shift m monthShift .|. d

-- Annoying to semi replicate this logic but otherwise we have to move much of this code to Internal to use the enums
daysInMonth :: Word16 -> Word16 -> [Word16]
daysInMonth 1 y
  | isLeap                                      = [1..29]
  | otherwise                                   = [1..28]
  where
    isLeap
      | 0 == y `mod` 100                        = False           -- 400+ is not possible here
      | otherwise                               = 0 == y `mod` 4
daysInMonth n _
  | n == 3 || n == 5 || n == 8 || n == 10       = [1..30]
  | otherwise                                   = [1..31]

hourShift :: Num a => a
hourShift = 12

minuteShift :: Num a => a
minuteShift = 6

encodeTime :: Word16 -> Word16 -> Word16 -> Word16
encodeTime h m s = shift h hourShift .|. shift m minuteShift .|. s

-- decode dates

yearMask :: Num a => a
yearMask = 65024

decodeYear :: Word16 -> Word16
decodeYear = flip shiftR yearShift . (.&.) yearMask

monthMask :: Num a => a
monthMask = 480

decodeMonth :: Word16 -> Word16
decodeMonth = flip shiftR monthShift . (.&.) monthMask

dayMask :: Num a => a
dayMask = 31

decodeDay :: Word16 -> Word16
decodeDay = (.&.) dayMask

-- decode time

hourMask :: Num a => a
hourMask = 61440

decodeHour :: Word16 -> Word16
decodeHour = flip shiftR hourShift . (.&.) hourMask

minuteMask :: Num a => a
minuteMask = 4032

decodeMinute :: Word16 -> Word16
decodeMinute = flip shiftR minuteShift . (.&.) minuteMask

secondMask :: Num a => a
secondMask = 63

decodeSecond :: Word16 -> Word16
decodeSecond = (.&.) secondMask