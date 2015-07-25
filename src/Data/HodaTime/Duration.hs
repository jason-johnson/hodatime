module Data.HodaTime.Duration
(
)
where

import Data.HodaTime.Instant (Duration(..), Instant(..))

days d = Duration $ Instant d 0 0

hours h = Duration $ Instant 0 (h * 60 * 60) 0

minutes m = Duration $ Instant 0 (m * 60) 0

seconds s = Duration $ Instant 0 s 0

milliseconds ms = Duration $ Instant 0 0 (ms * 1000000)

microseconds ms = Duration $ Instant 0 0 (ms * 1000)

nanoseconds ns = Duration $ Instant 0 0 ns