module Data.HTime.CacheTable
(
   DTCacheTable(..)
  ,cacheTable
)
where

import Data.Word (Word16)
import Data.Bits (shift, (.|.), (.&.), shiftR)

type DTCacheTableDaysEntry = Word16
type DTCacheTableHoursEntry = Word16

data DTCacheTable = DTCacheTable [DTCacheTableDaysEntry] [DTCacheTableDaysEntry] [DTCacheTableHoursEntry]

cacheTable :: DTCacheTable
cacheTable = DTCacheTable days negDays hours where
  days = firstYear ++ restYears
  firstYear = [ encodeDate 0 m d | m <- [3..12], d <- daysInMonth m 0]
  restYears = [ encodeDate y m d | y <- [1..127], m <- [1..12], d <- daysInMonth m y]
  negDays = 0 : negFirstYear ++ restPrevYears                                                   -- TODO: instead of 0 it should be undefined, as this should never be accessed
  negFirstYear = [ encodeDate 0 m d | m <- [2,1], d <- reverse . daysInMonth m $ 0]
  restPrevYears = [ encodeDate y m d | y <- [1..127], m <- [12,11..1], d <- reverse . daysInMonth m $ - y]   -- NOTE: all that matters is the feb calculation which is fixed by negating the year
  hours = [ encodeTime h m s | h <- [0..11], m <- [0..59], s <- [0..59]]

{-
secondsInTwelveHours = 43199

decodeFuture (DTCacheTable xs _ _) = map (\x -> (2000 + decodeYear x, decodeMonth x, decodeDay x)) xs
decodePast   (DTCacheTable _ xs _) = map (\x -> (2000 + (- decodeYear x), decodeMonth x, decodeDay x)) xs
decodeTime   (DTCacheTable _ _ xs) = map (\x -> (decodeHour x, decodeMinute x, decodeSecond x)) xs
decodeTime' t secs
  | secs > secondsInTwelveHours = adj . dt $ secs'
  | otherwise    = dt secs
  where
      secs' = secs - secondsInTwelveHours
      adj (h,m,s) = (12 + h,m,s)
      dt = (!!) . decodeTime $ t
-}

-- encode

yearShift :: Num a => a
yearShift = 9

monthShift :: Num a => a
monthShift = 5

encodeDate :: Word16 -> Word16 -> Word16 -> Word16
encodeDate y m d = shift y yearShift .|. shift m monthShift .|. d

daysInMonth :: Word16 -> Word16 -> [Word16]
daysInMonth 2 y
  | isLeap                                                               = [1..29]
  | otherwise                                                            = [1..28]
  where
    y' = y + 2000
    isLeap
      | 0 == y' `mod` 100 = 0 == y' `mod` 400
      | otherwise    = 0 == y' `mod` 4
daysInMonth n _
  | n == 4 || n == 6 || n == 9 || n == 11                                = [1..30]
  | otherwise                                                            = [1..31]

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