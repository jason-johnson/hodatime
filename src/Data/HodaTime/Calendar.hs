module Data.HodaTime.Calendar
(
   Calendar(..)
  ,foo
)
where

data Calendar = Iso | Gregorian
    deriving (Eq, Show)

foo :: Integer
foo = 1