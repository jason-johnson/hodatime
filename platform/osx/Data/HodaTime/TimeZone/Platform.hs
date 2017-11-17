module Data.HodaTime.TimeZone.Platform
(
   loadUTC
  ,fixedOffsetZone
  ,loadLocalZone
  ,loadTimeZone
)
where

import Data.HodaTime.TimeZone.Internal
import qualified Data.HodaTime.TimeZone.Unix as U

loadUTC :: IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
loadUTC = U.loadUTC loadZoneFromOlsonFile

fixedOffsetZone :: String -> Int -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap, TransitionInfo)
fixedOffsetZone = U.fixedOffsetZone loadLeaps

loadLocalZone :: IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap, String)
loadLocalZone = U.loadLocalZone loadZoneFromOlsonFile

loadTimeZone :: String -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
loadTimeZone = U.loadTimeZone loadZoneFromOlsonFile

loadZoneFromOlsonFile :: FilePath -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
loadZoneFromOlsonFile file = do
  (utcM, calDateM, leapM) <- U.defaultLoadZoneFromOlsonFile file
  leapM' <- loadLeaps leapM
  return (utcM, calDateM, leapM')

-- On Mac platform, leaps aren't stored anywhere so we have to load them seperately

loadLeaps :: LeapsMap -> IO LeapsMap
loadLeaps = return . mergeLeapMaps leaps
  where
    leaps = importLeaps []