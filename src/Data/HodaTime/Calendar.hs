module Data.HodaTime.Calendar
(
  Calendar(..)
)
where

data Calendar =
      Iso
    | Gregorian
        deriving (Eq, Show)