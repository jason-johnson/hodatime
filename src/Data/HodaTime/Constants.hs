module Data.HodaTime.Constants
(
   daysPerCycle
  ,daysPerCentury
  ,daysPerFourYears
  ,daysPerYear
  ,monthsPerYear
  ,daysPerWeek
  ,hoursPerDay
  ,minutesPerDay
  ,minutesPerHour
  ,secondsPerDay
  ,secondsInTwelveHours
  ,secondsPerHour
  ,secondsPerMinute
  ,usecsPerDay
  ,daysPerMonth
  ,monthDayOffsets
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

daysPerYear :: Num a => a
daysPerYear = 365

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

usecsPerDay :: Num a => a
usecsPerDay = 86400000000

daysPerMonth :: Num a => [a]
daysPerMonth = [31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 28]         -- NOTE: pre-rotated

monthDayOffsets :: Num a => [a]
monthDayOffsets = 0 : rest
  where
    rest = zipWith (+) daysPerMonth (0:rest)

-- conversion constants

unixDaysOffset :: Num a => a
unixDaysOffset = 11017
