module Data.HodaTime.Calendar.Gregorian
(
   WeekDay(..)
  ,Month(..)
  ,localDate
)
where

import Data.HodaTime.Constants
import Data.HodaTime.Types (LocalDate(..), Calendar(..))
import Data.Int (Int32)

-- types

data WeekDay = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show, Eq, Ord, Enum, Bounded)

data Month = January | February | March | April | May | June | July | August | September | October | November | December
  deriving (Show, Eq, Ord, Enum, Bounded)

-- constructors

localDate :: Int -> Month -> Int -> LocalDate
localDate year month day = LocalDate (fromIntegral year) (fromIntegral . fromEnum $ month) (fromIntegral day) Gregorian

-- helper functions
