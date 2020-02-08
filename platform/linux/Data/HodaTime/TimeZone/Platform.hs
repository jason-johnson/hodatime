module Data.HodaTime.TimeZone.Platform
(
   loadUTC
  ,loadLocalZone
  ,loadTimeZone
)
where

import Data.HodaTime.TimeZone.Internal
import qualified Data.HodaTime.TimeZone.Unix as U

loadUTC :: IO (UtcTransitionsMap, CalDateTransitionsMap)
loadUTC = U.loadUTC loadZoneFromOlsonFile

loadLocalZone :: IO (UtcTransitionsMap, CalDateTransitionsMap, String)
loadLocalZone = U.loadLocalZone loadZoneFromOlsonFile

loadTimeZone :: String -> IO (UtcTransitionsMap, CalDateTransitionsMap)
loadTimeZone = U.loadTimeZone loadZoneFromOlsonFile

loadZoneFromOlsonFile :: FilePath -> IO (UtcTransitionsMap, CalDateTransitionsMap)
loadZoneFromOlsonFile = U.defaultLoadZoneFromOlsonFile