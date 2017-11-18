module Data.HodaTime.Instant.Platform
(
  now
)
where

import Data.HodaTime.Instant.Internal
import qualified Data.HodaTime.Instant.Unix as U

now :: IO Instant
now = U.now
-- TODO: Do we need an inline statement here or will the compiler work it out?