module Data.HodaTime.Calendar
(
   Calendar(..)
  ,DayNth(..)
  ,minYear
  ,maxYear
)
where

-- TODO: Hide all these data types behind some constructors

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

data HebrewMonthNumbering =
    HebrewCivil
  | HebrewScriptural
    deriving (Eq, Show)

data Calendar =
    Iso
  | Gregorian           -- TODO: Add min week day thing?
  | Persian
  | Hebrew HebrewMonthNumbering
  | Coptic              -- TODO: Add min week day thing?
  | Islamic IslamicLeapYearPattern IslamicEpoch
    deriving (Eq, Show)

data DayNth =
    First
  | Second
  | Third
  | Fourth
  | Fifth
  | Last
  | SecondToLast
  | ThirdToLast
  | FourthToLast
    deriving (Eq, Show)

minYear :: Calendar -> Int
minYear = undefined

maxYear :: Calendar -> Int
maxYear = undefined
