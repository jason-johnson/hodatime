-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Persian.Astronomical
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- Astronomical determination of the Persian (Solar Hijri) new year, Nowruz.  The official Iranian calendar begins each
-- year on the day whose (apparent) noon at the reference meridian (52.5°E, i.e. Iran Standard Time, UTC+3:30) most
-- closely follows the March equinox: Nowruz is the day on which the equinox occurs if it is before true noon in Tehran,
-- otherwise the following day.
--
-- The equinox is computed with the method of Meeus (/Astronomical Algorithms/, ch. 27), corrected to Universal Time with
-- the Espenak–Meeus ΔT polynomials, and compared against true (apparent) noon using the equation of time (Meeus ch. 28).
-- The results are validated against the published modern Nowruz dates and the official leap-year sequence, and against
-- NodaTime's astronomical data (e.g. the epoch 1.Farvardin.1 = 22.Mar.622 CE, and the years where the astronomical and
-- arithmetic calendars diverge).  Accuracy is vouched for over 'minPersianYear' .. 'maxPersianYear'; the functions remain
-- total outside that range but the leap assignment there is an extrapolation.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Persian.Astronomical
(
   newYearDay
  ,minPersianYear
  ,maxPersianYear
)
where

import Data.Array.Unboxed (UArray, listArray, (!))

-- | First Persian year the calendar covers (the era begins in 622 CE).
minPersianYear :: Int
minPersianYear = 1

-- | Last Persian year for which the astronomical calendar is vouched for (≈ 2121 CE).  The equinox and ΔT models are
--   well grounded through this range.
maxPersianYear :: Int
maxPersianYear = 1500

-- | The universal flat day (day 0 = 1.Mar.2000 Gregorian, the 'Data.HodaTime.Instant.Instant' epoch) of Nowruz
--   (1 Farvardin) of the given Persian year.  Cached for the supported range and computed on demand outside it, so the
--   calendar is total for every year.
newYearDay :: Int -> Int
newYearDay y
  | y >= minPersianYear && y <= maxPersianYear + 1 = cache ! y
  | otherwise                                      = computeNewYearDay y

-- | Lazy cache of the new-year day for the supported range.  As a CAF it is built once, on the first Persian-calendar
--   operation, and never at all in programs that don't touch the Persian calendar.
cache :: UArray Int Int
cache = listArray (minPersianYear, maxPersianYear + 1) [computeNewYearDay y | y <- [minPersianYear .. maxPersianYear + 1]]
{-# NOINLINE cache #-}

-- | 1.Mar.2000 Gregorian (the universal flat day 0) as a Julian Day Number.
baseJDN :: Int
baseJDN = gregorianToJDN 2000 3 1

computeNewYearDay :: Int -> Int
computeNewYearDay pYear = nowruzJDN pYear - baseJDN

-- | The Julian Day Number of Nowruz for the given Persian year, via the astronomical rule described in the module header.
nowruzJDN :: Int -> Int
nowruzJDN pYear = if frac <= threshold then jdn else jdn + 1
  where
    gregYear = pYear + 621                              -- Nowruz of Persian year Y falls in Gregorian year Y + 621
    jde      = marchEquinoxJDE gregYear                 -- equinox in Terrestrial Time
    jdUT     = jde - deltaT gregYear / 86400            -- convert to Universal Time
    jdTehran = jdUT + 3.5 / 24                          -- Iran Standard Time (UTC+3:30, meridian 52.5°E)
    x        = jdTehran + 0.5                            -- shift so the integer part is the civil day, 0.5 = noon
    jdn      = floor x :: Int
    frac     = x - fromIntegral jdn                     -- fraction of the day from midnight (0.5 = mean noon)
    threshold = 0.5 - eotDays jde                       -- true (apparent) noon differs from mean noon by the equation of time

-- | The March (northward) equinox as a Julian Ephemeris Day (Terrestrial Time), per Meeus /Astronomical Algorithms/ ch. 27.
marchEquinoxJDE :: Int -> Double
marchEquinoxJDE year
  | year <= 1000 = correct $ 1721139.29189 + 365242.13740 * y1 + 0.06134 * y1 ** 2 + 0.00111 * y1 ** 3 - 0.00071 * y1 ** 4
  | otherwise    = correct $ 2451623.80984 + 365242.37404 * y2 + 0.05169 * y2 ** 2 - 0.00411 * y2 ** 3 - 0.00057 * y2 ** 4
  where
    y1 = fromIntegral year / 1000
    y2 = (fromIntegral year - 2000) / 1000
    correct jde0 = jde0 + (0.00001 * s) / dl
      where
        t  = (jde0 - 2451545.0) / 36525
        w  = 35999.373 * t - 2.47
        dl = 1 + 0.0334 * cos (d2r w) + 0.0007 * cos (d2r (2 * w))
        s  = sum [ a * cos (d2r (b + c * t)) | (a, b, c) <- periodicTerms ]

-- | The 24 periodic terms (A, B, C) of Meeus table 27.C, used to refine the mean equinox.
periodicTerms :: [(Double, Double, Double)]
periodicTerms =
  [ (485, 324.96,   1934.136), (203, 337.23,  32964.467), (199, 342.08,     20.186)
  , (182,  27.85, 445267.112), (156,  73.14,  45036.886), (136, 171.52,  22518.443)
  , ( 77, 222.54,  65928.934), ( 74, 296.72,   3034.906), ( 70, 243.58,   9037.513)
  , ( 58, 119.81,  33718.147), ( 52, 297.17,    150.678), ( 50,  21.02,   2281.226)
  , ( 45, 247.54,  29929.562), ( 44, 325.15,  31555.956), ( 29,  60.93,   4443.417)
  , ( 18, 155.12,  67555.328), ( 17, 288.79,   4562.452), ( 16, 198.04,  62894.029)
  , ( 14, 199.76,  31436.921), ( 12,  95.39,  14577.848), ( 12, 287.11,  31931.756)
  , ( 12, 320.81,  34777.259), (  9, 227.73,   1222.114), (  8,  15.45,  16859.074)
  ]

-- | ΔT (Terrestrial Time − Universal Time), in seconds, from the Espenak & Meeus polynomial expressions.
deltaT :: Int -> Double
deltaT yr
  | y < 500   = let u = y / 100         in 10583.6 - 1014.41 * u + 33.78311 * u^2 - 5.952053 * u^3 - 0.1798452 * u^4 + 0.022174192 * u^5 + 0.0090316521 * u^6
  | y < 1600  = let u = (y - 1000) / 100 in 1574.2 - 556.01 * u + 71.23472 * u^2 + 0.319781 * u^3 - 0.8503463 * u^4 - 0.005050998 * u^5 + 0.0083572073 * u^6
  | y < 1700  = let t = y - 1600 in 120 - 0.9808 * t - 0.01532 * t^2 + t^3 / 7129
  | y < 1800  = let t = y - 1700 in 8.83 + 0.1603 * t - 0.0059285 * t^2 + 0.00013336 * t^3 - t^4 / 1174000
  | y < 1860  = let t = y - 1800 in 13.72 - 0.332447 * t + 0.0068612 * t^2 + 0.0041116 * t^3 - 0.00037436 * t^4 + 0.0000121272 * t^5 - 0.0000001699 * t^6 + 0.000000000875 * t^7
  | y < 1900  = let t = y - 1860 in 7.62 + 0.5737 * t - 0.251754 * t^2 + 0.01680668 * t^3 - 0.0004473624 * t^4 + t^5 / 233174
  | y < 1920  = let t = y - 1900 in -2.79 + 1.494119 * t - 0.0598939 * t^2 + 0.0061966 * t^3 - 0.000197 * t^4
  | y < 1941  = let t = y - 1920 in 21.20 + 0.84493 * t - 0.076100 * t^2 + 0.0020936 * t^3
  | y < 1961  = let t = y - 1950 in 29.07 + 0.407 * t - t^2 / 233 + t^3 / 2547
  | y < 1986  = let t = y - 1975 in 45.45 + 1.067 * t - t^2 / 260 - t^3 / 718
  | y < 2005  = let t = y - 2000 in 63.86 + 0.3345 * t - 0.060374 * t^2 + 0.0017275 * t^3 + 0.000651814 * t^4 + 0.00002373599 * t^5
  | y < 2050  = let t = y - 2000 in 62.92 + 0.32217 * t + 0.005589 * t^2
  | y <= 2150 = -20 + 32 * ((y - 1820) / 100)^2 - 0.5628 * (2150 - y)
  | otherwise = let u = (y - 1820) / 100 in -20 + 32 * u^2
  where y = fromIntegral yr :: Double

-- | The equation of time (apparent − mean solar time), in days, at the given instant (Meeus ch. 28, low-accuracy form).
eotDays :: Double -> Double
eotDays jde = bigE / (2 * pi)
  where
    t   = (jde - 2451545.0) / 36525
    l0  = d2r $ 280.46646 + 36000.76983 * t + 0.0003032 * t^2
    m   = d2r $ 357.52911 + 35999.05029 * t - 0.0001537 * t^2
    e   = 0.016708634 - 0.000042037 * t - 0.0000001267 * t^2
    eps = d2r $ 23.439291 - 0.0130042 * t
    yy  = tan (eps / 2) ^ 2
    bigE = yy * sin (2 * l0) - 2 * e * sin m + 4 * e * yy * sin m * cos (2 * l0) - 0.5 * yy^2 * sin (4 * l0) - 1.25 * e^2 * sin (2 * m)

d2r :: Double -> Double
d2r x = x * pi / 180

-- | Julian Day Number for a proleptic Gregorian date.
gregorianToJDN :: Int -> Int -> Int -> Int
gregorianToJDN y m d = d + (153 * m' + 2) `div` 5 + 365 * y' + y' `div` 4 - y' `div` 100 + y' `div` 400 - 32045
  where
    a  = (14 - m) `div` 12
    y' = y + 4800 - a
    m' = m + 12 * a - 3
