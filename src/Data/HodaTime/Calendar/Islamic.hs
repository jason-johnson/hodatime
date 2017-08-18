module Data.HodaTime.Calendar.Islamic
(
)
where

data IslamicLeapYearPattern =
    ILYPBase15
  | ILYPBase16
  | ILYPIndian
  | ILYPHabashAlHasib
  deriving (Eq, Show)
    
data IslamicEpoch =
    IslamicAstronomical
  | IslamicCivil
  deriving (Eq, Show)