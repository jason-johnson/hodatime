-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Exceptions
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  TBD
--
-- Exceptions defined by this library.
----------------------------------------------------------------------------
module Data.HodaTime.Exceptions
(
  -- * Types
   ParseFailedException(..)
  ,DayRequiredException(..)
  ,MonthRequiredException(..)
  ,YearRequiredException(..)
  ,InvalidHourException(..)
  ,InvalidMinuteException(..)
  ,InvalidSecondException(..)
  ,InvalidNanoSecondException(..)
)
where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

-- Parsing

-- | Parse failed on the given string
data ParseFailedException = ParseFailedException String
  deriving (Typeable, Show)

-- | Parse failed on the given string (documentation defined on the instance - does this one show up?)
instance Exception ParseFailedException

-- | Day is required for parse pattern
data DayRequiredException = DayRequiredException
  deriving (Typeable, Show)

instance Exception DayRequiredException

-- | Month is required for parse pattern
data MonthRequiredException = MonthRequiredException
  deriving (Typeable, Show)

instance Exception MonthRequiredException

-- | Year is required for parse pattern
data YearRequiredException = YearRequiredException
  deriving (Typeable, Show)

instance Exception YearRequiredException

-- Constructor exceptions

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