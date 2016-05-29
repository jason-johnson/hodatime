module Data.HodaTime.Calendar
(
   Calendar(..)
  ,DayNth(..)
)
where

data Calendar =
      Iso
    | Gregorian
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