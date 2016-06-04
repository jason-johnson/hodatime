{-# LANGUAGE FlexibleInstances #-}

module Data.HodaTime.Calendar.Internal
(
)
where

import Data.HodaTime.LocalDateTime.Internal (LocalDate(..), LocalDateTime(..))

-- TODO: This will take more thought.  A LocalDate[Time] only makes sense within a specific calendar system.  This should be encoded in the type somehow.
-- TODO: Perhaps a type family, something like:

data CalDateTime c = CalDateTime (CalDate c) Int

data CalDate c = CalDate Int

data Gregorian = Gregorian
data Iso = Iso

class IsDateTime dt where
  next :: Int -> dt -> dt

instance IsDateTime (CalDate Gregorian) where
  next i dt = dt

instance IsDateTime (CalDate Iso) where
  next i dt = dt

instance IsDateTime (CalDateTime Gregorian) where
  next i (CalDateTime c ii) = CalDateTime (next i c) ii
