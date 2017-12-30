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

loadUTC :: IO (UtcTransitionsMap, CalDateTransitionsMap, Maybe TransitionExpressionDetails)
loadUTC = U.loadUTC loadZoneFromOlsonFile

fixedOffsetZone :: String -> Int -> (UtcTransitionsMap, CalDateTransitionsMap, Maybe TransitionExpressionDetails, TransitionInfo)
fixedOffsetZone = U.fixedOffsetZone

loadLocalZone :: IO (UtcTransitionsMap, CalDateTransitionsMap, Maybe TransitionExpressionDetails, String)
loadLocalZone = U.loadLocalZone loadZoneFromOlsonFile

loadTimeZone :: String -> IO (UtcTransitionsMap, CalDateTransitionsMap, Maybe TransitionExpressionDetails)
loadTimeZone = U.loadTimeZone loadZoneFromOlsonFile

loadZoneFromOlsonFile :: FilePath -> IO (UtcTransitionsMap, CalDateTransitionsMap, Maybe TransitionExpressionDetails)
loadZoneFromOlsonFile = U.defaultLoadZoneFromOlsonFile