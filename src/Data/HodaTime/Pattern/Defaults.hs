module Data.HodaTime.Pattern.Defaults
(
  DefaultForParse(..)
)
where

import Data.HodaTime.LocalTime.Internal (LocalTime(..))

class DefaultForParse d where
  getDefault :: d

instance DefaultForParse LocalTime where
  getDefault = LocalTime 0 0