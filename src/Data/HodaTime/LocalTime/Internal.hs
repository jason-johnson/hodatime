module Data.HodaTime.LocalTime.Internal
(
  fromInstant
)
where

import Data.HodaTime.Types (Instant(..), LocalTime(..))

fromInstant :: Instant -> LocalTime
fromInstant (Instant _ secs nsecs) = LocalTime secs nsecs