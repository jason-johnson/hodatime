module Data.HodaTime.LocalTime.Internal
(
   LocalTime(..)
  ,HasLocalTime(..)
  ,Hour
  ,Minute
  ,Second
  ,Nanosecond
  ,fromSecondsNormalized
  ,fromInstant  -- TODO: Remove
)
where

import Data.HodaTime.Instant.Internal (Instant(..))
import Data.HodaTime.CalendarDateTime.Internal (LocalTime(..), CalendarDateTime(..))
import Data.HodaTime.Internal (hoursFromSecs, minutesFromSecs, secondsFromSecs)
import Data.HodaTime.Constants (secondsPerDay)
import Data.Word (Word32)

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
      to = fromSecondsNormalized nsecs
  {-# INLINE hour #-}

  minute f (LocalTime secs nsecs) = minutesFromSecs to f secs
    where
      to = fromSecondsNormalized nsecs
  {-# INLINE minute #-}

  second f (LocalTime secs nsecs) = secondsFromSecs to f secs
    where
      to = fromSecondsNormalized nsecs
  {-# INLINE second #-}

  nanosecond f (LocalTime secs nsecs) = LocalTime secs . fromIntegral <$> (f . fromIntegral) nsecs
  {-# INLINE nanosecond #-}

instance HasLocalTime (CalendarDateTime cal) where
  hour f (CalendarDateTime cd lt) = CalendarDateTime cd <$> hour f lt
  minute f (CalendarDateTime cd lt) = CalendarDateTime cd <$> minute f lt
  second f (CalendarDateTime cd lt) = CalendarDateTime cd <$> second f lt
  nanosecond f (CalendarDateTime cd lt) = CalendarDateTime cd <$> nanosecond f lt

-- Constructors

fromInstant :: Instant -> LocalTime                             -- NOTE: This should never go to top level as Instant -> LocalTime is not supported, you must go through a ZonedDateTime
fromInstant (Instant _ secs nsecs) = LocalTime secs nsecs

fromSecondsNormalized :: Word32 -> Word32 -> LocalTime
fromSecondsNormalized nsecs = flip LocalTime nsecs . normalize
  where
    normalize x = if x >= secondsPerDay then x - secondsPerDay else x
