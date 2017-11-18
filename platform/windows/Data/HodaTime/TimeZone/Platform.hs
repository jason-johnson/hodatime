module Data.HodaTime.TimeZone.Platform
(
   loadUTC
  ,fixedOffsetZone
  ,loadLocalZone
  ,loadTimeZone
)
where

import Data.HodaTime.TimeZone.Internal

loadUTC :: IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
loadUTC = undefined

fixedOffsetZone :: String -> Int -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap, TransitionInfo)
fixedOffsetZone = undefined

loadLocalZone :: IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap, String)
loadLocalZone = undefined

loadTimeZone :: String -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
loadTimeZone = undefined