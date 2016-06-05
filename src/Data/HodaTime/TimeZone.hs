module Data.HodaTime.TimeZone
(
   utc
  ,local
  ,timeZoneAt
  ,atLeniently
  ,atStartOfDay
  ,atStrictly
  ,atAll
  ,resolve
  ,forOffset
  ,getUtcOffset
  ,maxOffset
  ,minOffset
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.LocalDateTime.Internal (LocalDateTime, LocalDate)
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime, ZoneLocalResult(..))
import Data.HodaTime.OffsetDateTime.Internal (Offset)

utc :: TimeZone
utc = UTCzone

timeZoneAt :: TZIdentifier -> Maybe TimeZone
timeZoneAt = undefined

local :: TimeZone
local = undefined

atLeniently :: LocalDateTime -> TimeZone -> ZonedDateTime
atLeniently = undefined

atStartOfDay :: LocalDate -> TimeZone -> ZonedDateTime
atStartOfDay = undefined

atStrictly :: LocalDateTime -> TimeZone -> Maybe ZonedDateTime
atStrictly ldt UTCzone = Just $ atLeniently ldt UTCzone
atStrictly _ldt _tz = undefined

-- | Return all 'ZonedDateTime' entries for a specific 'LocalDateTime' in a 'TimeZone'. Normally this would be one, but in the case that a time occurs twice in a zone (i.e. due to daylight savings time change)
-- | both would be returned.  Also, if the time does not occur at all, 'Nothing' will be returned.  This method allows the user to choose exactly what to do in the case of ambigiuty.
atAll :: LocalDateTime -> TimeZone -> Maybe ZoneLocalResult
atAll ldt UTCzone = Just . ZLSingle $ atLeniently ldt UTCzone
atAll _ldt _tz = undefined

-- | Takes two functions to determine how to resolve a 'LocalDateTime' to a 'ZonedDateTime' in the case of ambiguity or skipped times.  The first function is for the ambigous case and is past the first
-- | matching 'ZonedDateTime', followed by the second match. The second function is for the case that the 'LocalDateTime' doesn't exist in the 'TimeZone' (e.g. in a spring-forward situation, there will
-- | be a missing hour), the first 'ZonedDateTime' will be the the last time before the gap and the second will be the first time after the gap.
resolve :: LocalDateTime -> TimeZone -> (ZonedDateTime -> ZonedDateTime -> Maybe ZonedDateTime) -> (ZonedDateTime -> ZonedDateTime -> Maybe ZonedDateTime) -> Maybe ZonedDateTime
resolve ldt UTCzone _ _ = Just $ atLeniently ldt UTCzone
resolve _ldt _tz _am _sk = undefined

-- | Return a special 'ZonedDateTime' for the given 'Offset'.  The identifier will be "UTC" in the case of a zero 'Offset' and "UTC(+/-)Offset" otherwise.
forOffset :: LocalDateTime -> Offset -> TimeZone
forOffset = undefined

getUtcOffset :: TimeZone -> Int
getUtcOffset UTCzone = 0
getUtcOffset _tz = undefined

maxOffset :: TimeZone -> Int
maxOffset UTCzone = 0
maxOffset _tz = undefined

minOffset :: TimeZone -> Int
minOffset UTCzone = 0
minOffset _tz = undefined
