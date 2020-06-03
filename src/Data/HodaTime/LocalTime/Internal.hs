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

import Data.HodaTime.CalendarDateTime.Internal (LocalTime(..), CalendarDateTime(..), CalendarDate, day, IsCalendar(..))
import Data.HodaTime.Internal (hoursFromSecs, minutesFromSecs, secondsFromSecs, secondsFromHours, secondsFromMinutes)
import Data.HodaTime.Constants (secondsPerDay)
import Data.Functor.Identity (Identity(..))
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
  -- | Lens for the hour component of the 'LocalTime'
  hour :: Functor f => (Hour -> f Hour) -> lt -> f lt
  -- | Lens for the minute component of the 'LocalTime'
  minute :: Functor f => (Minute -> f Minute) -> lt -> f lt
  -- | Lens for the second component of the 'LocalTime'
  second :: Functor f => (Second -> f Second) -> lt -> f lt
  -- | Lens for the nanoseconds component of the 'LocalTime'.  NOTE: no effort is made to detect nano overflow.  They will simply roll over on overflow without affecting the rest of the time.
  nanosecond :: Functor f => (Nanosecond -> f Nanosecond) -> lt -> f lt

instance HasLocalTime LocalTime where
  hour f (LocalTime secs nsecs) = hoursFromSecs to f secs
    where
      to = fromSecondsClamped nsecs
  {-# INLINE hour #-}

  minute f (LocalTime secs nsecs) = minutesFromSecs to f secs
    where
      to = fromSecondsClamped nsecs
  {-# INLINE minute #-}

  second f (LocalTime secs nsecs) = secondsFromSecs to f secs
    where
      to = fromSecondsClamped nsecs
  {-# INLINE second #-}

  nanosecond f (LocalTime secs nsecs) = LocalTime secs . fromIntegral <$> (f . fromIntegral) nsecs
  {-# INLINE nanosecond #-}

instance IsCalendar cal => HasLocalTime (CalendarDateTime cal) where
  hour f (CalendarDateTime cd (LocalTime secs nsecs)) = hoursFromSecs to f secs
    where
      to = fromSecondsRolled cd nsecs
  {-# INLINE hour #-}

  minute f (CalendarDateTime cd (LocalTime secs nsecs)) = minutesFromSecs to f secs
    where
      to = fromSecondsRolled cd nsecs
  {-# INLINE minute #-}

  second f (CalendarDateTime cd (LocalTime secs nsecs)) = secondsFromSecs to f secs
    where
      to = fromSecondsRolled cd nsecs
  {-# INLINE second #-}

  nanosecond f (CalendarDateTime cd lt) = CalendarDateTime cd <$> nanosecond f lt
  {-# INLINE nanosecond #-}

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
      date' = if d == 0 then date else runIdentity . day (Identity . (+ fromIntegral d)) $ date  -- NOTE: inlining the modify lens here

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