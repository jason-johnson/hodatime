module Data.HodaTime.LocalTime.Internal
(
   LocalTime(..)
  ,HasLocalTime(..)
  ,Hour
  ,Minute
  ,Second
  ,Nanosecond
  ,localTime
  ,midnight
  ,InvalidHourException(..)
  ,InvalidMinuteException(..)
  ,InvalidSecondException(..)
  ,InvalidNanoSecondException(..)
)
where

import Data.HodaTime.CalendarDateTime.Internal (LocalTime(..), CalendarDateTime(..), CalendarDate, day, setDay, IsCalendar(..))
import Data.HodaTime.Internal (secondsFromHours, secondsFromMinutes)
import Data.HodaTime.Constants (secondsPerDay)
import Data.Word (Word32)
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Exception (Exception)
import Data.Typeable (Typeable)

-- Exceptions

-- | Given hour was not valid
data InvalidHourException = InvalidHourException
  deriving (Typeable, Show)

instance Exception InvalidHourException

-- | Given minute was not valid
data InvalidMinuteException = InvalidMinuteException
  deriving (Typeable, Show)

instance Exception InvalidMinuteException

-- | Given second was not valid
data InvalidSecondException = InvalidSecondException
  deriving (Typeable, Show)

instance Exception InvalidSecondException

-- | Given nanosecond was not valid
data InvalidNanoSecondException = InvalidNanoSecondException
  deriving (Typeable, Show)

instance Exception InvalidNanoSecondException

-- Types

type Hour = Int
type Minute = Int
type Second = Int
type Nanosecond = Int

class HasLocalTime lt where
  hour :: lt -> Hour
  setHour :: Hour -> lt -> lt
  minute :: lt -> Minute
  setMinute :: Minute -> lt -> lt
  second :: lt -> Second
  setSecond :: Second -> lt -> lt
  nanosecond :: lt -> Nanosecond
  setNanosecond :: Nanosecond -> lt -> lt

instance HasLocalTime LocalTime where
  hour (LocalTime secs _) = fromIntegral (secs `div` 3600)
  {-# INLINE hour #-}
  setHour value (LocalTime secs nsecs) = fromSecondsClamped nsecs (replaceHour value secs)

  minute (LocalTime secs _) = fromIntegral (secs `mod` 3600 `div` 60)
  {-# INLINE minute #-}
  setMinute value (LocalTime secs nsecs) = fromSecondsClamped nsecs (replaceMinute value secs)

  second (LocalTime secs _) = fromIntegral (secs `mod` 60)
  {-# INLINE second #-}
  setSecond value (LocalTime secs nsecs) = fromSecondsClamped nsecs (replaceSecond value secs)

  nanosecond (LocalTime _ nsecs) = fromIntegral nsecs
  {-# INLINE nanosecond #-}
  setNanosecond value (LocalTime secs _) = LocalTime secs (fromIntegral value)

instance IsCalendar cal => HasLocalTime (CalendarDateTime cal) where
  hour (CalendarDateTime _ lt) = hour lt
  {-# INLINE hour #-}
  setHour value (CalendarDateTime cd (LocalTime secs nsecs)) = fromSecondsRolled cd nsecs (replaceHour value secs)

  minute (CalendarDateTime _ lt) = minute lt
  {-# INLINE minute #-}
  setMinute value (CalendarDateTime cd (LocalTime secs nsecs)) = fromSecondsRolled cd nsecs (replaceMinute value secs)

  second (CalendarDateTime _ lt) = second lt
  {-# INLINE second #-}
  setSecond value (CalendarDateTime cd (LocalTime secs nsecs)) = fromSecondsRolled cd nsecs (replaceSecond value secs)

  nanosecond (CalendarDateTime _ lt) = nanosecond lt
  {-# INLINE nanosecond #-}
  setNanosecond value (CalendarDateTime cd lt) = CalendarDateTime cd (setNanosecond value lt)

-- NOTE: AM/PM is handled in the pattern layer (see Data.HodaTime.Pattern.LocalTime): the
--       designator and the 12-hour hour each rewrite only their half of the 'hour' via div/mod 12, which keeps
--       them order independent when composed.

-- | Private function for constructing a localtime at midnight
midnight :: LocalTime
midnight = LocalTime 0 0

-- helper functions

fromSecondsClamped :: Word32 -> Word32 -> LocalTime
fromSecondsClamped nsecs = flip LocalTime nsecs . normalize
  where
    normalize x = if x >= secondsPerDay then x - secondsPerDay else x

fromSecondsRolled :: IsCalendar cal => CalendarDate cal -> Word32 -> Word32 -> CalendarDateTime cal
fromSecondsRolled date nsecs secs = CalendarDateTime date' $ LocalTime secs' nsecs
    where
      (d, secs') = secs `divMod` secondsPerDay
      date' = if d == 0 then date else setDay (day date + fromIntegral d) date

replaceHour :: Hour -> Word32 -> Word32
replaceHour value secs = secs - (secs `div` 3600 * 3600) + fromIntegral value * 3600

replaceMinute :: Minute -> Word32 -> Word32
replaceMinute value secs = secs - (secs `mod` 3600 `div` 60 * 60) + fromIntegral value * 60

replaceSecond :: Second -> Word32 -> Word32
replaceSecond value secs = secs - secs `mod` 60 + fromIntegral value

-- constructors

-- | Create a new 'LocalTime' from an hour, minute, second and nanosecond if values are valid
localTime :: MonadThrow m => Hour -> Minute -> Second -> Nanosecond -> m LocalTime
localTime h m s ns = do
  unless (h < 24 && h >= 0) $ throwM InvalidHourException
  unless (m < 60 && m >= 0) $ throwM InvalidMinuteException
  unless (s < 60 && m >= 0) $ throwM InvalidSecondException
  unless (ns >= 0) $ throwM InvalidNanoSecondException
  return $ LocalTime (h' + m' + fromIntegral s) (fromIntegral ns)
  where
    h' = secondsFromHours h
    m' = secondsFromMinutes m