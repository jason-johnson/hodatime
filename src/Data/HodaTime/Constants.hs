module Data.HodaTime.Constants
(
   hoursPerDay
  ,minutesPerDay
  ,minutesPerHour
  ,secondsPerDay
  ,secondsInTwelveHours
  ,secondsPerHour
  ,secondsPerMinute
  ,millisecondsPerSecond
  ,microsecondsPerSecond
  ,nsecsPerSecond
  ,nsecsPerMicrosecond
  ,unixDaysOffset
)
where

-- Time constants

hoursPerDay :: Num a => a
hoursPerDay = 24

minutesPerDay :: Num a => a
minutesPerDay = 1440

minutesPerHour :: Num a => a
minutesPerHour = 60

secondsPerDay :: Num a => a
secondsPerDay = 86400

secondsInTwelveHours :: Num a => a
secondsInTwelveHours = 43200

secondsPerHour :: Num a => a
secondsPerHour = 3600

secondsPerMinute :: Num a => a
secondsPerMinute = 60

millisecondsPerSecond :: Num a => a
millisecondsPerSecond = 1000

microsecondsPerSecond :: Num a => a
microsecondsPerSecond = 1000000

nsecsPerSecond :: Num a => a
nsecsPerSecond = 1000000000

nsecsPerMicrosecond :: Num a => a
nsecsPerMicrosecond = 1000

-- conversion constants

unixDaysOffset :: Num a => a
unixDaysOffset = 11017
