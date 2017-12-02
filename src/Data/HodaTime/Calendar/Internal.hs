module Data.HodaTime.Calendar.Internal
(
   zellers
  ,zellersNthDay
  ,daysInMonth
)
where

-- TODO: Do we still need this?

-- NOTE: Sunday = 0, January = 0
zellers :: Int -> Int -> Int -> Int
zellers d month year = (d + (13 * m - 1) `div` 5 + yrhs + (yrhs `div` 4) + (ylhs `div` 4) - 2 * ylhs) `mod` 7
  where
    (m, year') = if month < 2 then (month + 11, year - 1) else (month - 1, year)
    yrhs = year' `mod` 100
    ylhs = year' `div` 100

zellersNthDay :: Int -> Int -> Int -> Int -> Int
zellersNthDay nth d m y = day
  where
    dom = if nth < 0 then mdm else 1
    mdm = daysInMonth m y
    dow = zellers dom m y
    d' = d - dow
    d'' = if d' < 0 then d' + 7 else d'
    day = dom + d'' + 7 * nth

daysInMonth :: (Show a, Integral a) => a -> a -> a
daysInMonth m y
  | m == 12   = if isLeap then 29 else 28
  | m < 12    = 30 + ((m + m `div` 6) `mod` 2) +  (m `div` 11)
  | otherwise = error $ "daysInMonth called with invalide month: " ++ show m
  where
    isLeap
       | 0 == y `mod` 100                  = 0 == y `mod` 400
       | otherwise                         = 0 == y `mod` 4