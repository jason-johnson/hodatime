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
   DayRequiredException(..)
  ,MonthRequiredException(..)
  ,YearRequiredException(..)
)
where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

-- Parsing

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