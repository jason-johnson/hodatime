module Data.HodaTime.LocalDateTime
(
  on
)
where

import Data.HodaTime.LocalDateTime.Internal

on :: LocalTime  -> LocalDate -> LocalDateTime
on time date = LocalDateTime date time