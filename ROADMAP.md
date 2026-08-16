# Roadmap

This document records possible improvements to HodaTime identified by comparing
it with iotaTime, its Idris 2 port. It is a design inventory, not a release plan
or commitment to implement every item.

The comparison was made against HodaTime 1.1.0.0 at commit `5199c05` and the
corresponding iotaTime roadmap. Each entry distinguishes an implementation gap
from a speculative API change. Proposed names and signatures are illustrative
until an implementation is accepted.

## Status and impact

- **Confirmed** means iotaTime implements the capability and HodaTime does not
  expose an equivalent.
- **Qualified** means the central difference is real, but the original claim
  needs narrower wording.
- **Not currently a gap** means the comparison did not establish an iotaTime
  capability missing from HodaTime.
- **Additive** changes can normally be introduced without invalidating existing
  source code.
- **Behavioral** changes preserve source compatibility but can change results or
  failures.
- **Breaking** changes alter existing types or signatures.
- **Platform** changes affect native Unix or Windows support and packaging.

No ordering below implies implementation priority.

## 1. Date and local difference operations

**Status:** Confirmed  
**Likely impact:** Additive

### Proposal

Add operations for measuring the difference between two `CalendarDate` values,
two `LocalTime` values, and two `CalendarDateTime` values.

Date differences need two distinct meanings:

- an exact count of elapsed calendar days; and
- a calendar-aware decomposition into years, months, and days.

The calendar-aware form should apply units from largest to smallest and use the
same clamped month arithmetic as period application. Its result must not
overshoot the end date. Local-time and calendar-date-time differences should
measure their supported fields exactly rather than silently treating a calendar
month as a fixed duration.

### What this fixes

HodaTime can apply a `Period`, but callers cannot ask the inverse question in a
supported way. Applications currently have to inspect dates manually or write
calendar-specific subtraction code.

### API effect

This would add functions such as `betweenDays`, `between`, or
`betweenWith` to the date and local date/time modules. Exact day differences
should return an integral count; decomposed differences should return `Period`
values constrained to the relevant target type. Existing arithmetic would be
unchanged.

### Open decisions

- Define argument order consistently with `Instant.difference`.
- Decide whether callers select units through separate functions or an explicit
  unit set.
- Specify behavior when calendars differ; rejecting the operation is safer than
  converting implicitly.

## 2. Validated bounded intervals and relationships

**Status:** Confirmed  
**Likely impact:** Additive initially; behavioral or breaking if the existing
constructor and lenses are changed

### Proposal

Require a bounded half-open interval `[start, end)` to satisfy `start <= end`,
and add:

- emptiness, overlap, adjacency, and connectedness predicates;
- intersection of overlapping intervals;
- union of overlapping or adjacent intervals; and
- distinct failures for a reversed interval, empty intersection, and
  disconnected union.

### What this fixes

The current `interval` constructor and endpoint lenses can create reversed
intervals. Once that happens, `contains` and `duration` no longer describe a
valid half-open range. HodaTime also lacks the common relationship and set
operations needed to combine intervals safely.

### API effect

The compatibility-preserving route is to add a validated constructor returning
`Maybe Interval` or `Either IntervalError Interval`, then add pure relationship
queries and partial set operations returning typed results. Replacing the
existing constructor with a checked result would be a breaking change.

The writable `start` and `end` lenses present a second compatibility problem:
independent endpoint updates cannot preserve the invariant. Long term, they
would need to become read-only observations or be supplemented by validated
replacement functions.

### Open decisions

- Whether to deprecate or immediately change the unchecked constructor.
- Whether empty intervals have an intersection with themselves. iotaTime's
  intersection operation specifically requires a non-empty result.
- Whether error values should include the offending endpoints.

## 3. Unbounded intervals

**Status:** Confirmed  
**Likely impact:** Additive

### Proposal

Introduce a separate half-open interval type whose start, end, or both can be
unbounded. A missing start means negative infinity and a missing end means
positive infinity.

The type should support validated construction, conversion to and from bounded
`Interval`, containment, emptiness, overlap, adjacency, intersection, connected
union, and duration when both endpoints are finite.

### What this fixes

Many validity windows are naturally one-sided: "effective from this instant"
or "valid until this instant." Callers currently need sentinel instants or a
custom wrapper, both of which obscure the actual domain.

### API effect

This is best represented by a new `UnboundedInterval` type rather than changing
`Interval`. Endpoint observations would return `Maybe Instant`, bounded
conversion would return `Maybe Interval`, and duration would return
`Maybe Duration`. Existing interval code would remain source-compatible.

## 4. Civil-epoch Islamic calendar

**Status:** Confirmed  
**Likely impact:** Additive, with type-level calendar API expansion

### Proposal

Add the civil epoch for the tabular Islamic calendar while retaining the
existing leap-year pattern selection. Astronomical-epoch and civil-epoch dates
should have distinct types so they cannot be mixed accidentally.

### What this fixes

Tabular Islamic calendars use more than one epoch convention. HodaTime supports
the astronomical epoch, forcing users of the civil convention either to apply
manual day adjustments or to use the wrong calendar identity.

### API effect

The Islamic calendar type would gain an epoch parameter or distinct exported
calendar aliases. Existing astronomical calendar names should remain aliases
for their current meaning to avoid silently changing dates.

### Open decisions

- Whether the epoch is represented by a promoted data type or separate calendar
  marker types.
- Naming that makes both the epoch and leap pattern visible without making
  ordinary signatures unwieldy.

## 5. Injectable clocks

**Status:** Confirmed  
**Likely impact:** Additive

### Proposal

Introduce an explicit clock abstraction with system and fixed implementations,
plus an adapter that combines a clock with a time zone to obtain current zoned
date-times.

### What this fixes

Application code using `now` is coupled to the process clock. Tests must arrange
real-time boundaries or thread timestamps through unrelated code. A fixed clock
makes such tests deterministic without adding mutable global state.

### API effect

`Instant.now` should remain as the convenient system operation. New code could
accept a `Clock` value or typeclass constraint and call an observation such as
`getCurrentInstant`. A `ZonedClock` would hold a clock and zone and expose the
corresponding current `ZonedDateTime`.

### Open decisions

- A first-class record of actions is easier to inject; a typeclass is easier to
  use through constraints. The library should not expose both without a reason.
- Mutable fake clocks should remain application/test utilities unless a strong
  core-library use case emerges.

## 6. Time-zone provider contract

**Status:** Confirmed  
**Likely impact:** Additive; Platform

### Proposal

Represent a source of time-zone data explicitly instead of binding all public
loading operations directly to the operating system. A provider should supply:

- UTC;
- lookup by identifier;
- the configured local zone;
- available identifiers; and
- provider metadata.

System operations would become convenience wrappers over the system provider.

### What this fixes

An explicit provider enables deterministic tests, embedded TZDB data, database
backends, snapshots, and applications that cannot or should not read process
configuration. It also establishes one place to specify identifier and metadata
semantics.

### API effect

Add an opaque `TimeZoneProvider` and operations such as `timeZoneWith`,
`localZoneWith`, and `availableZonesWith`. Preserve `timeZone`, `localZone`, and
`availableZones` by implementing them through `systemTimeZoneProvider`.

Provider failures should use a typed error result rather than relying on
platform exceptions. This introduces a new non-throwing API; changing the
existing functions would be a separate breaking decision.

## 7. Provider caching and Windows snapshots

**Status:** Confirmed  
**Likely impact:** Additive; Platform; operational caching semantics

### Proposal

Add caller-owned, opt-in caching around a time-zone provider. The policy should
independently select caching for named zones, available-zone discovery,
metadata, and the local zone. Cache successful results only.

On Windows, add a provider that reads the registry once and serves an immutable
snapshot thereafter.

### What this fixes

Repeated native discovery and zone loading can be expensive, but a mandatory
global cache creates unclear ownership and stale local-zone behavior. An
explicit wrapper makes lifetime and freshness application decisions. A Windows
snapshot also ensures all reads in a test or operation use one coherent registry
view.

### API effect

Add `TimeZoneCachePolicy`, a provider-wrapping operation that allocates caches,
and `windowsSnapshotTimeZoneProvider`. No existing loader needs to change.

### Open decisions

- Whether concurrent cache fill is single-flight or permits duplicate loads.
- Whether failures are never cached, or whether negative caching is separately
  configurable.
- Whether the local zone belongs in the default cache policy.

## 8. Offset and zone preserving conversions

**Status:** Confirmed  
**Likely impact:** Additive

### Proposal

Add the following missing conversions:

- `OffsetDateTime` to its represented `Instant`;
- `OffsetDateTime` to another calendar;
- `OffsetDateTime` to another offset while preserving its instant; and
- `ZonedDateTime` to another zone while preserving its instant.

### What this fixes

HodaTime can construct an `OffsetDateTime` from an instant but does not expose
the inverse. Callers also cannot change the displayed offset or zone without
manually extracting and rebuilding values. Manual reconstruction risks
preserving local fields when the intended invariant is the instant.

### API effect

Add `toInstant`, `withCalendar`, and `withOffset` to
`Data.HodaTime.OffsetDateTime`, and `withZone` to
`Data.HodaTime.ZonedDateTime`. In Haskell these conversions can be total while
the target calendars retain the current representable range; if calendar ranges
later diverge, the signatures may need a failure result.

The documentation must distinguish instant-preserving `withOffset` from merely
replacing an offset label, which would change the represented instant.

## 9. Fixed-duration zoned arithmetic

**Status:** Confirmed  
**Likely impact:** Additive; behavior around time-zone transitions must be
specified precisely

### Proposal

Add and subtract `Duration` values from `ZonedDateTime` on the global timeline,
then query the zone again at the resulting instant to determine the new offset
and local time.

### What this fixes

Elapsed-time arithmetic and civil-time arithmetic differ at daylight-saving
transitions. Adding 24 elapsed hours can produce a different local clock time,
whereas adding one calendar day normally preserves local time. HodaTime does not
currently provide the elapsed-time operation directly on a zoned value.

### API effect

Add `add` and `minus` functions in `Data.HodaTime.ZonedDateTime` accepting
`Duration`. They should be equivalent to converting to `Instant`, applying
instant arithmetic, and calling `fromInstant` with the original zone.

This must remain distinct from any future `Period` operation on zoned values,
which requires skipped/ambiguous local-time resolution policy.

## 10. Direct zone-state queries

**Status:** Confirmed  
**Likely impact:** Additive; exposes a new stable abstraction over time-zone
internals

### Proposal

Allow callers to query a `TimeZone` directly for the wall offset or complete
zone interval active at an `Instant`. A zone interval should expose:

- optional half-open start and end bounds;
- wall offset;
- exact daylight-saving adjustment when known;
- whether daylight saving is active; and
- abbreviation.

### What this fixes

Today callers must construct a `ZonedDateTime` to inspect some zone state, and
cannot ask for the transition interval containing an instant. That makes
transition-aware scheduling, diagnostics, and cache invalidation unnecessarily
difficult.

### API effect

Add an opaque `ZoneInterval` plus observations and functions analogous to
`zoneOffsetAt :: TimeZone -> Instant -> Offset` and
`zoneIntervalAt :: TimeZone -> Instant -> ZoneInterval`.

The abstraction should not expose the fingertree or platform representation of
`TimeZone`; the interval contract, however, would become public and stable.

## 11. TZDB identity metadata and platform mappings

**Status:** Qualified  
**Likely impact:** Additive; Platform

### Proposal

Expose provider metadata containing the TZDB version and alias-to-canonical-ID
links, together with canonical identifier resolution. Decide separately whether
raw IANA/Windows conversion operations should be public.

### What this fixes

Version metadata allows diagnostics and reproducibility checks. Alias data lets
applications normalize identifiers without loading a zone merely to discover
its identity.

### Qualification

iotaTime publicly exposes version, alias, and canonicalization metadata, which
HodaTime lacks. However, its Windows/IANA conversion functions are platform
implementation details rather than public API. HodaTime already performs
equivalent ICU conversion internally on Windows. The original comparison
therefore overstates the mapping gap.

### API effect

`TimeZoneProvider` could return a `TzdbMetadata` value with an optional version
and a list or map of aliases. A pure `canonicalZoneId` observation would resolve
alias chains.

Public `ianaToWindowsZone` and `windowsToIanaZone` operations should be a
separate proposal. Exposing them introduces platform availability, CLDR region,
and failure semantics that metadata alone does not require.

## 12. Arithmetic Persian calendar variants

**Status:** Confirmed  
**Likely impact:** Additive, with additional calendar types

### Proposal

Add type-distinct arithmetic Persian calendars for:

- the legacy simple 33-year cycle; and
- Ahmad Birashk's 2820-year cycle.

Retain the current vouched astronomical Persian calendar as a separate type.
The arithmetic variants can support complete years 1 through 9377 because they
do not depend on the bounded astronomical table.

### What this fixes

The existing astronomical calendar is intentionally limited to years 1 through
1500 and cannot represent applications that explicitly require either
arithmetic rule. Treating these rules as interchangeable would silently produce
different leap years.

### API effect

Add calendar marker types such as `PersianSimple` and `PersianArithmetic`, with
constructors and runtime refiners specific to each. Shared month names can
remain common, but dates under different rules must not unify at the type level.
The current `Persian` name and behavior should remain unchanged.

## 13. Typed, non-throwing pattern failures

**Status:** Qualified  
**Likely impact:** Additive if introduced alongside existing parsing;
breaking if existing `parse` signatures change

### Proposal

Add non-throwing parsing operations returning structured errors that distinguish
at least:

- unexpected end of input;
- unexpected characters;
- invalid numbers;
- values outside supported ranges;
- trailing input;
- invalid completed values;
- provider failures; and
- local-time resolver failures.

Every parse-originated failure should retain the source position at which it
became known.

### What this fixes

HodaTime currently reduces parse failures to a thrown
`ParseFailedException String`. Callers cannot reliably inspect the failure,
compose it with provider/resolver errors, or use ordinary sum-type handling.

### Qualification

iotaTime provides typed, non-throwing parsing, but only four of its six core
`PatternError` constructors currently carry positions. Its range and completed
value failures do not. The proposed HodaTime contract should either preserve
positions for all parser-originated failures or describe the narrower guarantee
honestly.

### API effect

Preserve `parse` and `parse'` for compatibility and add explicitly non-throwing
variants returning `Either PatternError value`. Changing the existing
polymorphic `MonadThrow` functions would be breaking. Zoned parsing should wrap
layout, parse, provider, and resolution failures in distinct constructors rather
than flattening them to text.

## 14. Composable scalar patterns

**Status:** Confirmed  
**Likely impact:** Additive

### Proposal

Add reusable patterns for:

- exact instant nanoseconds;
- calendar-relative day numbers; and
- quoted and token-delimited time-zone identifiers.

### What this fixes

HodaTime's field patterns focus on human calendar/time layouts. Applications
building machine-oriented or mixed representations must write parsing and
formatting logic outside the pattern system, losing bidirectional composition.

### API effect

Add a scalar pattern module or place the patterns in their corresponding value
modules. They should compose with existing literal and pair combinators. Exact
instant nanoseconds depend on resolving the representation decision in item 17.

## 15. Explicit `strftime` layout compilers

**Status:** Not currently a gap  
**Likely impact:** None unless deliberately promoted as a new API in both
libraries

### Original proposal

Expose functions that compile caller-supplied `strftime` layouts into patterns
for dates, times, date-times, and offset date-times.

### Audit result

Both HodaTime and iotaTime contain explicit layout compiler functions as private
implementation details. Both publicly expose helpers that compile the layouts
stored in a `Locale`. iotaTime therefore does not currently have the public API
that the original claim proposed porting.

### Possible future API

Promoting the private compilers may still be useful, but it is a shared design
proposal rather than an iotaTime parity item. Doing so would expose the supported
specifier subset and `StrftimeError` behavior as compatibility commitments.

Before promotion, decide whether unsupported specifiers return `Either`, throw
through `MonadThrow`, or both, and whether zone-bearing layouts are compiled or
handled only by specialized zoned/offset entry points.

## 16. Typed locale acquisition

**Status:** Confirmed  
**Likely impact:** Additive if retained alongside current functions; Platform

### Proposal

Add locale acquisition operations that return typed failures, distinguishing an
unavailable locale from a platform access failure.

### What this fixes

`currentLocale` and `localeByName` currently report failure through exceptions.
Applications that expect missing optional locales must catch exceptions rather
than handle a normal data result.

### API effect

Add a `LocaleError` type and non-throwing variants returning
`IO (Either LocaleError Locale)`. Existing `MonadThrow` operations can remain as
convenience wrappers. Replacing them outright would be breaking.

## 17. Exact arbitrary-precision observations and period readers

**Status:** Confirmed  
**Likely impact:** Period readers are additive; arbitrary-precision instant
support is potentially breaking and affects performance, instances, and native
boundaries

### Proposal

Expose every `Period` component and provide exact round-trippable instant
observations in nanoseconds using arbitrary-precision integers.

### What this fixes

HodaTime's `Period` constructor is hidden and its record selectors are not
exported, so a caller can construct but not inspect a period. `Instant` is stored
as fixed-width day, second, and nanosecond fields and exposes no exact scalar
nanosecond conversion. iotaTime can round-trip every integer nanosecond value by
construction.

### API effect

Exporting `periodYears` through `periodNanoseconds` is straightforward and does
not expose the `Period` constructor.

Exact arbitrary-precision instant round trips are a deeper decision. Merely
returning `Integer` from the current fixed-width representation gives an exact
observation only within HodaTime's existing bounded range; accepting every
`Integer` requires changing `Instant` representation and arithmetic. That can
alter overflow behavior, memory use, performance, `Hashable` results, native
conversion checks, and assumptions in time-zone structures.

### Open decisions

- Whether the goal is an exact scalar view of the existing bounded domain or an
  actually unbounded `Instant` domain.
- Which epoch the scalar uses and whether Unix-epoch nanoseconds are provided
  separately.
- Whether a representation change warrants a major version.

## 18. Whole-unit duration and Unix-second readers

**Status:** Confirmed  
**Likely impact:** Additive; quotient semantics must be documented

### Proposal

Complement duration constructors with observations in microseconds,
milliseconds, seconds, minutes, hours, standard days, and standard weeks. Also
add whole seconds since the Unix epoch for `Instant`.

### What this fixes

A duration constructed from hours cannot currently be read back in hours or any
other whole unit through the public API. Callers cannot perform ordinary
serialization, reporting, or threshold checks without retaining the original
input separately.

### API effect

Add `toMicroseconds`, `toMilliseconds`, `toSeconds`, `toMinutes`, `toHours`,
`toStandardDays`, and `toStandardWeeks` to `Data.HodaTime.Duration`, plus
`toSecondsSinceUnixEpoch` to `Data.HodaTime.Instant`.

The return type should initially match the representable range or use `Integer`
to avoid overflow during unit conversion. Negative non-integral values require
an explicit rounding rule; Unix seconds commonly use floor division, but that
must not be left implicit.

## 19. Correct `localTime` validation

**Status:** Confirmed defect  
**Likely impact:** Behavioral bug fix

### Proposal

Correct `localTime` so seconds are accepted only in `[0, 59]` and nanoseconds
only in `[0, 999999999]`. Add lower- and upper-bound tests for every component.

### What this fixes

The current second validation checks `s < 60 && m >= 0`; the lower-bound test
uses the already-validated minute value instead of the second value. Negative
seconds can therefore pass. Nanoseconds are checked only for non-negativity, so
values of one second or greater are accepted without normalization.

Those values violate the documented component ranges and can create local times
whose observations are not canonical.

### API effect

The type and signature of `localTime` do not change. Calls that currently
succeed with invalid negative seconds or oversized nanoseconds will begin
throwing `InvalidSecondException` or `InvalidNanoSecondException`. This is an
intentional behavioral correction.

Tests should cover `-1`, each maximum valid value, and the first value above the
maximum for hour, minute, second, and nanosecond.

## Shared opportunity: richer provider discovery

**Status:** Absent from both libraries  
**Likely impact:** Additive; Platform

### Proposal

Consider richer controls than `availableZones` only when deployment or profiling
demonstrates a need. Possible capabilities include canonical-only discovery,
aliases, metadata without loading zones, platform source identification, and
filtered or incremental enumeration.

### Why this is not yet a concrete item

Neither library currently supplies these controls, and their useful shape
depends on the provider's data source. A filesystem TZDB, embedded snapshot,
Windows registry, and remote database do not necessarily support the same
discovery operations efficiently. The provider contract should be established
before extending it.

## Suggested sequencing if work begins

The following order reduces risk; it is not a commitment or priority ranking.

1. Correct `localTime` validation and add boundary tests.
2. Add observations and conversions that do not change representations:
   duration readers, period readers, `OffsetDateTime.toInstant`, `withOffset`,
   and `withZone`.
3. Add injectable clocks and the provider contract while preserving existing
   convenience operations.
4. Add validated bounded and unbounded interval APIs, then decide separately
   how to deprecate unchecked construction and writable endpoint lenses.
5. Add calendar variants and direct zone-state queries.
6. Treat arbitrary-precision `Instant` and any replacement of throwing APIs as
   versioned compatibility projects rather than incidental additions.
