module Data.HodaTime.LocalDate
(
  on
)
where

import Data.HodaTime.Types (LocalTime, LocalDate, LocalDateTime(..))

on :: LocalTime  -> LocalDate -> LocalDateTime
on time date = LocalDateTime date time