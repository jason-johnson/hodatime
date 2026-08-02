[![Build Status](https://github.com/jason-johnson/hodatime/actions/workflows/haskell.yml/badge.svg)](https://github.com/jason-johnson/hodatime)
[![Hackage](https://img.shields.io/hackage/v/hodatime.svg)](https://hackage.haskell.org/package/hodatime)

# hodatime

A full-featured date and time library for Haskell, inspired by [Noda Time](https://nodatime.org) (and, in turn, Joda Time) and Erik Naggum's *The Long, Painful History of Time*.

hodatime draws a clear line between **physical time** — instants, durations and intervals on a single global timeline — and **civil time** — calendar dates, local times, and the calendar-and-zone rules that connect the two. The type you are holding always tells you exactly what you can and cannot do with it, so whole classes of date/time mistakes simply do not type-check.

## Features

- **Physical vs. civil time**, kept distinct at the type level (`Instant` / `Duration` / `Interval` versus `CalendarDate` / `LocalTime` / `CalendarDateTime`).
- **Multiple calendars** — Gregorian, ISO, Julian, Coptic, Persian (astronomical Solar Hijri), Islamic (parameterised over the leap-year rule) and Hebrew — with lossless conversion between them.
- **Operating-system time zones** — the IANA/Olson database on Unix, the registry on Windows — with explicit resolution of skipped and ambiguous local times across DST transitions.
- **A type-safe, composable pattern system** that derives a parser *and* a formatter together from a single field definition.
- **Locale-aware formatting** driven by the host system's locale data.

## Example

```haskell
import Data.HodaTime.Calendar.Gregorian (calendarDate, Month(..))
import Data.HodaTime.CalendarDate (dayOfWeek, withCalendar, CalendarDate)
import Data.HodaTime.Pattern (format)
import Data.HodaTime.Pattern.CalendarDate (pR)
import qualified Data.HodaTime.Calendar.Julian as Julian

-- Dates are smart-constructed; a date that cannot exist is Nothing
valentines = calendarDate 14 February 2024     -- a valid date (Just ...)
notADate   = calendarDate 30 February 2024     -- Nothing

-- Read-only components are plain functions
valentinesDoW = dayOfWeek <$> valentines       -- Just Wednesday

-- Convert losslessly to another calendar (the target is chosen by the result type)
julianChristmas :: Maybe (CalendarDate Julian.Julian)
julianChristmas = withCalendar <$> calendarDate 25 December 2024   -- 12 December 2024 in the Julian calendar

-- Format with a pattern (pR is the ISO-8601 round-trip date pattern)
isoText = format pR <$> calendarDate 23 April 2024                 -- Just "2024-04-23"
```

## Installing

```
cabal install hodatime
```

or add `hodatime` to the `build-depends` of your package.

## Documentation

The full API documentation is on [Hackage](https://hackage.haskell.org/package/hodatime). Start with the [`Data.HodaTime`](https://hackage.haskell.org/package/hodatime/docs/Data-HodaTime.html) module, which gives a guided tour of the core concepts.

## Changelog

Release notes are published on the [GitHub releases page](https://github.com/jason-johnson/hodatime/releases).

## Versioning

Git release tags are the version source of truth. On a commit tagged `vX.Y.Z.W`,
`scripts/sync-version.sh` sets the Cabal package version to `X.Y.Z.W`; later
commits use `X.Y.Z.(W+1)`. CI runs the script before building or publishing.
Run it locally after creating a tag to synchronize `hodatime.cabal`.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
