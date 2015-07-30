module Data.HodaTime.Calendar.Gregorian.Internal
(
  fromInstant
)
where

import Data.HodaTime.Types (Instant(..), LocalDate(..))

fromInstant :: Instant -> LocalDate
fromInstant (Instant days _ _) = undefined

