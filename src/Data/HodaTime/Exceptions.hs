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
)
where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

-- | Exception for when a parse fails on the given string
data ParseFailedException = ParseFailedException String
  deriving (Typeable, Show)

-- | Exception for when a parse fails on the given string (documentation defined on the instance - does this one show up?)
instance Exception ParseFailedException

-- | Exception for when a parse fails on the given string
data DayRequiredException = DayRequiredException
  deriving (Typeable, Show)

instance Exception DayRequiredException

-- | Exception for when a parse fails on the given string
data MonthRequiredException = MonthRequiredException
  deriving (Typeable, Show)

instance Exception MonthRequiredException

-- | Exception for when a parse fails on the given string
data YearRequiredException = YearRequiredException
  deriving (Typeable, Show)

instance Exception YearRequiredException