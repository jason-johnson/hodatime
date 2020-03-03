module Data.HodaTime.Pattern.ZonedDateTime
(
  -- TODO: We don't expose these, they are building blocks
   pat_day
  ,pat_month
  ,pat_year
)
where

import Data.HodaTime.Pattern.Internal
import Data.HodaTime.CalendarDateTime.Internal (IsCalendar)
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime(..))
import qualified  Data.Text as T
import qualified  Data.Text.Lazy.Builder as TLB
import Control.Applicative ((<|>))
import Text.Parsec (digit, count, string, choice, oneOf, char)
import Formatting (left, (%.), later)

pat_year :: IsCalendar cal => Int -> Pattern (ZonedDateTime cal -> ZonedDateTime cal) (ZonedDateTime cal -> String) String
pat_year c = undefined

pat_month :: IsCalendar cal => Int -> Pattern (ZonedDateTime cal -> ZonedDateTime cal) (ZonedDateTime cal -> String) String
pat_month = undefined

pat_day :: IsCalendar cal => Int -> Pattern (ZonedDateTime cal -> ZonedDateTime cal) (ZonedDateTime cal -> String) String
pat_day = undefined