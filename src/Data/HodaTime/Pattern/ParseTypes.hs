-- Intermediate types used by the parser portion of the Pattern
module Data.HodaTime.Pattern.ParseTypes
(
   TimeInfo(..)
  ,hour
  ,minute
  ,second
  ,DateInfo(..)
  ,DateTimeInfo(..)
  ,ZonedDateTimeInfo(..)
)
where

import Data.HodaTime.CalendarDateTime.Internal (Month)

data TimeInfo = TimeInfo
  {
     _hour :: Int
    ,_minute :: Int
    ,_second :: Int
    ,_nanoSecond :: Int
  }

hour :: Functor f => (Int -> f Int) -> TimeInfo -> f TimeInfo
hour k ti = fmap (\h -> ti { _hour = h }) (k (_hour ti))

minute :: Functor f => (Int -> f Int) -> TimeInfo -> f TimeInfo
minute k ti = fmap (\m -> ti { _minute = m }) (k (_minute ti))

second :: Functor f => (Int -> f Int) -> TimeInfo -> f TimeInfo
second k ti = fmap (\s -> ti { _second = s }) (k (_second ti))

data DateInfo cal = DateInfo
  {
     day :: Maybe Int
    ,month :: Maybe (Month cal)
    ,year :: Maybe Int
  }

data DateTimeInfo cal = DateTimeInfo
  {
     date :: DateInfo cal
    ,time :: TimeInfo
  }

data ZonedDateTimeInfo cal = ZonedDateTimeInfo
  {
     dateTime :: DateTimeInfo cal
    ,zone :: String
  }