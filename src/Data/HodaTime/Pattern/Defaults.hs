module Data.HodaTime.Pattern.Defaults
(
  DefaultForParse(..)
)
where

import Data.HodaTime.LocalTime.Internal (LocalTime(..))
import Data.HodaTime.CalendarDateTime.Internal (IsCalendar, CalendarDate(..), CalendarDateTime(..), at)

class DefaultForParse d where
  getDefault :: d

instance DefaultForParse LocalTime where
  getDefault = LocalTime 0 0

instance IsCalendar cal => DefaultForParse (CalendarDate cal) where
  getDefault = CalendarDate 0 1 2 2000

instance IsCalendar cal => DefaultForParse (CalendarDateTime cal) where
  getDefault = getDefault `at` getDefault