module Data.HodaTime.Constants
(
   daysPerCycle
  ,daysPerCentury
  ,daysPerFourYears
  ,monthsPerYear
  ,daysPerWeek
  ,hoursPerDay
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

daysPerCycle :: Num a => a      -- NOTE: A "cycle" is 400 years
daysPerCycle = 146097

daysPerCentury :: Num a => a
daysPerCentury = 36524

daysPerFourYears :: Num a => a
daysPerFourYears = 1461

monthsPerYear :: Num a => a
monthsPerYear = 12

daysPerWeek :: Num a => a
daysPerWeek = 7

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
