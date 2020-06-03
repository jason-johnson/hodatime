-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Duration
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  TBD
--
-- A 'Pattern' is used to parse and format types in this library.
----------------------------------------------------------------------------
module Data.HodaTime.Pattern
(
  -- * Types
   Pattern(..)
  -- * Parsing / Formatting
  ,parse
  ,parse'
  ,format
  -- * Pattern operators
  ,(<%)
  ,(%>)
  -- * Patterns
  ,pat_string
  ,pat_char
  -- * Exceptions
  ,ParseFailedException
)
where

import Data.HodaTime.Pattern.Internal