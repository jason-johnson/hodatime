{-# LANGUAGE FunctionalDependencies #-}
module Data.HodaTime.Pattern.ApplyParse
(
   DefaultForParse(..)
  ,ApplyParse(..)
  ,ZonedDateTimeInfo(..)
  ,DateTimeInfo(..)
  ,TimeInfo(..)
  ,DateInfo(..)
)
where

import Data.HodaTime.LocalTime.Internal (LocalTime(..), localTime)
import Data.HodaTime.CalendarDateTime.Internal (IsCalendar, CalendarDate(..), CalendarDateTime(..), at)
import Data.HodaTime.Pattern.ParseTypes
import Control.Monad.Catch (MonadThrow)

defaultTime = TimeInfo 0 0 0 0

defaultDate = DateInfo Nothing Nothing Nothing

defaultDateTime = DateTimeInfo defaultDate defaultTime

-- Class

class DefaultForParse d where
  getDefault :: d

instance DefaultForParse LocalTime where
  getDefault = LocalTime 0 0

instance IsCalendar cal => DefaultForParse (CalendarDate cal) where
  getDefault = CalendarDate 0 1 2 2000

instance IsCalendar cal => DefaultForParse (CalendarDateTime cal) where
  getDefault = getDefault `at` getDefault


class ApplyParse a b | b -> a where
  applyParse :: MonadThrow m => (a -> a) -> m b

instance ApplyParse TimeInfo LocalTime where
  applyParse f = localTime (_hour ti) (_minute ti) (_second ti) (_nanoSecond ti)
    where
      ti = f defaultTime

instance IsCalendar cal => ApplyParse (DateInfo cal) (CalendarDate cal) where
  applyParse f = undefined

{- class ApplyParse r where
  type StartData r
  getStartData :: StartData r
  applyParse :: StartData r -> r

instance ApplyParse LocalTime where
  type StartData LocalTime = TimeInfo
  getStartData = defaultTime

instance IsCalendar cal => ApplyParse (CalendarDate cal) where
  type StartData LocalTime = DateInfo cal
  getStartData = defaultDate

instance IsCalendar cal => ApplyParse (CalendarDateTime cal) where
  type StartData r = DateTimeInfo
  getStartData = defaultDateTime

instance IsCalendar cal => ApplyParse (ZonedDateTimeInfo cal) where
  type StartData r = ZonedDateTimeInfo
  getStartData = defaultDateTime "UTC" -}