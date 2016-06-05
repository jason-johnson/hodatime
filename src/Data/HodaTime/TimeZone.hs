module Data.HodaTime.TimeZone
(
   utc
  ,local
  ,timeZoneAt
  ,atLeniently
  ,atStartOfDay
  ,atStrictly
  ,getUtcOffset
  ,maxOffset
  ,minOffset
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.LocalDateTime.Internal (LocalDateTime, LocalDate)
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime)

timeZoneAt :: TZIdentifier -> Maybe TimeZone
timeZoneAt = undefined

utc :: TimeZone
utc = UTCzone

local :: TimeZone
local = undefined

atLeniently :: LocalDateTime -> TimeZone -> ZonedDateTime
atLeniently = undefined

atStartOfDay :: LocalDate -> TimeZone -> ZonedDateTime
atStartOfDay = undefined

atStrictly :: LocalDateTime -> TimeZone -> Maybe ZonedDateTime
atStrictly ldt UTCzone = Just $ atLeniently ldt UTCzone
atStrictly _ldt _tz = undefined

getUtcOffset :: TimeZone -> Int
getUtcOffset UTCzone = 0
getUtcOffset _tz = undefined

maxOffset :: TimeZone -> Int
maxOffset UTCzone = 0
maxOffset _tz = undefined

minOffset :: TimeZone -> Int
minOffset UTCzone = 0
minOffset _tz = undefined
