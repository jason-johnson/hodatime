module Data.HTime.Constants
(
   daysPerYear
  ,monthsPerYear
  ,daysPerWeek
  ,hoursPerDay
  ,minutesPerDay
  ,minutesPerHour
  ,secondsPerDay
  ,secondsPerHour
  ,secondsPerMinute
  ,usecsPerDay
  ,daysPerMonth
  ,monthDayOffsets
  ,unixDaysOffset
)
where

-- Time constants

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

secondsPerHour :: Num a => a
secondsPerHour = 3600

secondsPerMinute :: Num a => a
secondsPerMinute = 60

usecsPerDay :: Num a => a
usecsPerDay = 86400000000

daysPerMonth :: Num a => [a]
daysPerMonth = [31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 28]

monthDayOffsets :: Num a => [a]
monthDayOffsets = zipWith (+) daysPerMonth (0:monthDayOffsets)

-- conversion constants

unixDaysOffset :: Num a => a
unixDaysOffset = 11017