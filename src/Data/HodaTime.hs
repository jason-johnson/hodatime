{-|
Module      :  Data.HodaTime
Copyright   :  (C) 2017 Jason Johnson
License     :  BSD-style (see the file LICENSE)
Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
Stability   :  experimental
Portability :  POSIX, Windows

HodaTime is a Date and Time library that aims to be fully featured, convenient and type safe.

= Overview

This guide provides documentation to complement the API reference. It is recommended that you read at least the first few sections before starting to develop using Hoda Time.
If you have suggestions or questions which are likely to be discussion-based, please create an issue on Github. For more specific solution-focused questions,
please ask on Stack Overflow using the hodatime tag.

= Why does Hoda Time exist?

Hodatime was inspired by Erik Naggum's "Long painful history of time" and the C# library Noda Time which, itself, was inspired by Java's
Joda Time.  Noda Time tried to improve upon Joda Time by improving type saftey.  With Hodatime we seek to use the more advanced Haskell type
system to improve this safty even further.  For example, every attempt is made to avoid runtime errors by making wrong code impossible
to compile.  Failing that, a type is returned to force the user to deal with the fact that the call can fail.

== Why not just use Data.Time?

The Data.Time library is very well thought out and high quality.  For us, the issue is that the library can feel anemic.  We have no doubt that it has the
building blocks for anything one would wish to do with Dates or Time but for most real world functionality there would be more code required than is provided
by the base library.  This leads to two common situations: everyone implements their own versions of the missing "extras" or someone creates an additional package
for eveyone to use.  An example of the latter is that in most real world scenarios proper Time Zone handling will be required.  To do this with Data.Time practically
one will need __two__ additional package dependancies.  We prefer a more "batteries included" approach.  We we find that the "many fine-grained packages" strategy
puts an extra burden on the developer to know exactly what functionality is needed and include only those fine-grained packages and no more.  We prefer to have
fully defined packages and to let the compiler remove any unused code.

== What about leap seconds?

At the time of this writing, Hoda Time does not support leap seconds.  We are not opposed to leap seconds, but have not yet determined a practical way to include
them.  Leap seconds are required to properly represent how time works in the real world but it puts some rather large limitations on code that uses it.  For example,
dates more than six months into the future would be ill-defined (and thus, should be impossible to create) as we cannot predict what leap seconds will occur.  If we
include leap seconds they must be practical, safe and convenient and obvious for users to utilize.

== Design Style

=== Naming

"There are only two hard things in Computer Science: cache invalidation and naming things." -- Phil Karlton

In Hodatime we attempt to lower the burden of naming things by using the simplest name that is correct.  One consequence of this style
is that we use lots of modules.  This way functionality that is fundamentally the same but differs due to some context can share the
same name but occupy a different module.  This allows the user to decide what they would like the "context" part to be named instead
of imposing this on every user by embedding the context in the function name.  We see this use of modules as a positive as most
languages behave similiarly and handling imports is something a proper IDE can generally handle for us.

=== Accessors

For access the convention is simple: read-only accessors are just functions and all read/write accessors are valid lenses.  We incur
no dependancy on any lens library but the accessors are defined
<https://github.com/ekmett/lens/wiki/How-can-I-write-lenses-without-depending-on-lens%3F here>.  The user of the library can use their
favorite lens library or define 3 simple functions (see tests/HodaTime/Util.hs) if they do not wish to use any existing library.

= How to use this library

== Core Concepts

Almost everything in Hoda Time follows from a single distinction: the difference between /physical time/ and /civil time/.

/Physical time/ is what a stopwatch measures.  It flows at the same rate everywhere, it has no notion of days, months or time zones, and any two observers can agree on it.  A single point on this universal timeline is an @Instant@ (see "Data.HodaTime.Instant"), and the amount of time elapsed between two instants is a @Duration@ (see "Data.HodaTime.Duration").  These are the types to reach for when the question is /"how long did this take?"/ or /"which of these two events happened first?"/ — they cannot mislead you about time zones because they know nothing about them.

/Civil time/ is the human labelling laid on top of that timeline: calendars, wall clocks, "the 23rd of April at nine in the morning".  A label like that is not, on its own, a point on the timeline.  Until you say /where/ it applies it is ambiguous — "9am on the 23rd" happens at different physical instants in Tokyo and in New York.  Hoda Time gives that unanchored label its own type, @CalendarDateTime@ (see "Data.HodaTime.CalendarDateTime"), and deliberately makes it /not/ interchangeable with an @Instant@.  You move between the two worlds on purpose — by supplying the missing information, either a fixed @Offset@ from UTC or a full @TimeZone@ — and never by accident.

This split is the most important idea in the library.  A great many date and time bugs come from treating a wall-clock label as though it were an absolute instant; Hoda Time turns that mistake into a compile error instead of a lurking one.

=== The pieces

Each concept below has its own type and module.  You rarely need all of them at once — start with the one that matches the question you are asking, and follow the links for the detail.

[@Instant@ — "Data.HodaTime.Instant"] A single point on the universal timeline, independent of any calendar or zone.

[@Duration@ — "Data.HodaTime.Duration"] The exact time elapsed between two instants, measured in days, hours, seconds and nanoseconds.  This is /machine/ time — a precise count — as opposed to a calendar-aware amount such as "one month", whose length depends on which month you mean.

[@LocalTime@ — "Data.HodaTime.LocalTime"] A time of day on its own, such as 09:00:00, with no date attached.

[@CalendarDate@ — "Data.HodaTime.CalendarDate"] A date in some calendar, such as 23 April 2024, with no time of day attached.

[@CalendarDateTime@ — "Data.HodaTime.CalendarDateTime"] A date together with a time of day, still /not/ tied to any particular place on the timeline.

[@Offset@ — "Data.HodaTime.Offset"] A fixed displacement from UTC, such as +01:00.

[@OffsetDateTime@ — "Data.HodaTime.OffsetDateTime"] A @CalendarDateTime@ pinned to the timeline by a fixed @Offset@: enough to be unambiguous, but with no knowledge of daylight saving.

[@TimeZone@ — "Data.HodaTime.TimeZone"] The full set of rules for a place, including its history of daylight-saving and offset changes.

[@ZonedDateTime@ — "Data.HodaTime.ZonedDateTime"] A date and time anchored in a real @TimeZone@ — the fully resolved civil time, which therefore also corresponds to a definite @Instant@.

[@Interval@ — "Data.HodaTime.Interval"] The span of physical time between two instants, as a value you can hold and inspect.

[The calendar — "Data.HodaTime.Calendar.Gregorian" and friends] The system of dates itself.  Gregorian is the default, but Julian, Coptic, Persian, Islamic, Hebrew and ISO are all provided; the calendar is carried in the type, so dates from different calendars cannot be silently mixed.

[Patterns — "Data.HodaTime.Pattern"] Parsing text into these types, and formatting them back out again.

=== A first example

Here is the whole library in miniature.  A meeting is scheduled for 9 in the morning on 23 April 2024 in Zürich, and we want the exact instant at which it happens — and what that same moment reads on a wall clock in New York.

> import Data.HodaTime.Calendar.Gregorian (calendarDate, Month(..), Gregorian)
> import Data.HodaTime.LocalTime (localTime)
> import Data.HodaTime.CalendarDateTime (at)
> import Data.HodaTime.TimeZone (timeZone)
> import Data.HodaTime.ZonedDateTime (ZonedDateTime, fromCalendarDateTimeStrictly, toInstant, fromInstant, zoneAbbreviation)
> import Data.HodaTime.Pattern (format)
> import Data.HodaTime.Pattern.CalendarDateTime (pF)
> import Data.HodaTime.Pattern.ZonedDateTime (zonedDateTimePattern)
>
> main :: IO ()
> main = do
>   zurich  <- timeZone "Europe/Zurich"
>   newYork <- timeZone "America/New_York"
>
>   -- civil time: a date and a time of day, combined into a label with no place on the timeline
>   let Just meeting = at <$> calendarDate 23 April 2024 <*> localTime 9 0 0 0
>
>   -- anchor the label in Zürich, then read off the physical instant
>   here <- fromCalendarDateTimeStrictly meeting zurich
>   let instant = toInstant here
>
>   -- the very same instant, on a New York wall clock (any calendar would do, so we name Gregorian)
>   let there = fromInstant instant newYork :: ZonedDateTime Gregorian
>
>   -- one zoned pattern renders both: the long civil form (pF) followed by the zone abbreviation
>   let zoned = zonedDateTimePattern pF (\z -> " " ++ zoneAbbreviation z)
>   putStrLn $ format zoned here    -- Tuesday, 23 April 2024 09:00:00 CEST
>   putStrLn $ format zoned there   -- Tuesday, 23 April 2024 03:00:00 EDT

Read top to bottom, the example crosses from civil time to physical time and back:

1. @calendarDate@ and @localTime@ build /civil/ values.  Both return a @Maybe@, because 31 April and 25:00 are not real; combining them with @at@ gives a @CalendarDateTime@ — a label that does not yet name a point on the timeline.

2. @fromCalendarDateTimeStrictly@ resolves that label inside a @TimeZone@, producing a @ZonedDateTime@ that /is/ anchored, and @toInstant@ extracts the pure @Instant@.  We used the /strict/ resolver, which refuses to guess when a local time is skipped or ambiguous — the awkward cases covered in the section on offsets and zones.

3. Because an @Instant@ is just a point on the timeline, @fromInstant@ can re-express it on any zone's wall clock.  New York is six hours behind Zürich in April, so 09:00 CEST is 03:00 EDT.

4. Finally a /pattern/ renders each @ZonedDateTime@ as text: @zonedDateTimePattern@ pairs the long civil form (@pF@) with the zone abbreviation, so a single @format@ prints the whole line.  Patterns are the subject of their own section below.

Notice the type annotation on the New York view: @fromInstant@ can hand back a date in /any/ calendar, so we name the one we want.  That the calendar rides along in the type — and never has to be guessed — is the subject of the section on calendars.

=== Physical time

The three physical-time types — @Instant@, @Duration@ and @Interval@ — have one thing in common: they know nothing of calendars or time zones.  They are pure points and lengths on the universal timeline, and every operation on them is exact.

__Instant.__  An @Instant@ is a single moment, the same everywhere.  You will usually get one from the outside world with @now@ (an @IO Instant@), from a @ZonedDateTime@ with @toInstant@, or from a raw count of seconds with @fromSecondsSinceUnixEpoch@.  To read a moment /as/ a date and time you must first choose a zone: @inTimeZone@ turns an @Instant@ into a @ZonedDateTime@.  This is the same rule as everywhere else in the library — there is no calendar view of a moment until you say where you are standing.

__Duration.__  A @Duration@ is the exact gap between two instants, tracked down to the nanosecond.  You build one from a unit — @fromHours@, @fromMinutes@, @fromSeconds@, @fromNanoseconds@ and friends — and shift an instant by it with @add@ and @minus@ (a worked example is in the cookbook on "Data.HodaTime.Duration").

The word /standard/ in @fromStandardDays@ and @fromStandardWeeks@ is a deliberate caution.  A standard day is /exactly/ 24 hours and a standard week /exactly/ seven of them, because a @Duration@ is machine time.  That is not the same thing as "a calendar day": across a daylight-saving change a civil day can run to 23 or 25 hours.  If you mean "the same wall-clock time tomorrow" you are asking a /calendar/ question and should add to a @ZonedDateTime@; if you mean "exactly 24 hours later" you add a @Duration@ to an @Instant@.  Keeping those two apart is, once more, the entire point.

__Interval.__  An @Interval@ is a stretch of the timeline fixed by its two endpoints, built with @interval start end@.  It is /half-open/: @contains@ counts the start instant but not the end, so that adjacent intervals tile the timeline without overlapping.  @duration@ returns its length as a @Duration@, and its @start@ and @end@ are exposed as lenses.

=== Civil time

Civil time is the world of labels: a @CalendarDate@ is a date, a @LocalTime@ is a time of day, and a @CalendarDateTime@ is the two together.  None of them names a point on the timeline — that is what makes them /civil/ rather than /physical/ — and each carries its calendar in its type, so a Gregorian date and a Hebrew date can never be mistaken for one another.

__CalendarDate.__  You build a date through the module for the calendar you want, most often "Data.HodaTime.Calendar.Gregorian".  Its @calendarDate@ takes a day, a month and a year and returns a @Maybe@, because a great many (day, month, year) triples are not real dates: 30 February, the 31st of a 30-day month, 29 February in a common year, or — since the Gregorian calendar is not proleptic — anything before the changeover of 15 October 1582.  Rather than silently "fixing" your input, the library hands back @Nothing@ and lets you decide what that should mean.  Two further constructors cover common calendar idioms: @fromNthDay@ for "the fourth Thursday of November", and @fromWeekDate@ for week-numbered dates.  (Worked examples are in the cookbook on "Data.HodaTime.CalendarDate".)

__LocalTime.__  A @LocalTime@ is a wall-clock time with no date, built with @localTime@ from an hour, minute, second and nanosecond.  Clock arithmetic /normalizes/: adding one minute to 23:59 rolls round to 00:00 rather than overflowing, so a @LocalTime@ is always a real time of day.

__CalendarDateTime.__  Glue a date and a time together with @at@ (or its flipped partner @on@) to get a @CalendarDateTime@, and @atStartOfDay@ pairs a date with midnight.  This is still a civil label — the very @CalendarDateTime@ we anchored in the opening example — and it becomes a point on the timeline only once you resolve it against an @Offset@ or a @TimeZone@.

__Reading and changing fields.__  Every date type is an instance of @HasDate@, which offers its components in two forms.  The read-only accessors — @month@, @dayOfWeek@ and the combined @yearMonthDay@ — are ordinary functions.  The mutable components — @day@, @monthl@ (the month as an @Int@, so that arithmetic on it is meaningful) and @year@ — are /lenses/, while @next@ and @previous@ jump to the nth following or preceding weekday.  The lenses are quietly opinionated about the awkward cases:

* @day@ does /not/ clamp: add 400 to it and the month and year roll over accordingly.
* @monthl@ clamps only as a final step, and only for end-of-month days — so two months after 31 January is 31 March, not the 29 March that some libraries would hand you.
* @year@ clamps 29 February back to the 28th in a common year.

Hoda Time takes no dependency on any lens library to provide these; as noted under /Accessors/ above, any lens package will drive them, or you can define the three one-line helpers from @tests\/HodaTime\/Util.hs@ if you would rather not pull one in.

=== Crossing over: offsets and zones

A civil label becomes a point on the timeline only once you attach the missing UTC information, and there are two ways to attach it.  You can give a /fixed/ displacement from UTC — an @Offset@ — or the /full rules/ of a place — a @TimeZone@.  Each produces an anchored type: an @OffsetDateTime@ or a @ZonedDateTime@.

__Offset and OffsetDateTime.__  An @Offset@ is a fixed distance from UTC, such as +02:00, built with @fromHours@, @fromMinutes@, @fromSeconds@ or @empty@ (UTC itself) and adjusted through its @hours@, @minutes@ and @seconds@ lenses.  Offsets are clamped to a maximum of eighteen hours either side of UTC, comfortably covering every real zone.  An @OffsetDateTime@ (from @fromCalendarDateTimeWithOffset@ or @fromInstantWithOffset@) is simply a @CalendarDateTime@ tagged with one — the shape HTTP and other wire formats use, as in @2024-04-23T09:00:00+02:00@.  It is unambiguous, but /dumb/: it records that the offset was +02:00 without knowing that +01:00 applies in winter.  Reach for it when the offset is already a given (a timestamp on the wire, a logged event); reach for a @TimeZone@ when the question involves a place and its rules.

__TimeZone and ZonedDateTime.__  A @TimeZone@ is the whole rulebook for a location — every daylight-saving and offset change in its history.  Because that data comes from the operating system, loading a zone is an @IO@ action: @timeZone "Europe\/Zurich"@, @utc@, @localZone@ for the machine's own setting, or @availableZones@ to list them all.  Resolving a civil date and time in a zone yields a @ZonedDateTime@: a fully pinned-down value that corresponds to exactly one @Instant@.  It answers every question — @year@, @month@, @day@, @hour@ and the rest, plus @inDst@ and @zoneAbbreviation@ — converts to the timeline with @toInstant@ and comes back with @fromInstant@ (equivalently @inTimeZone@).

__Two awkward moments.__  Turning a /local/ @CalendarDateTime@ into a @ZonedDateTime@ is not always a one-to-one mapping, because twice a year the clocks move:

* On the /spring-forward/ night an hour of local time is __skipped__ — a label such as 02:30 simply never occurs, and maps to /no/ instant.
* On the /fall-back/ night an hour is repeated, so a label is __ambiguous__ — 01:30 happens /twice/, mapping to two different instants.

Most libraries quietly pick an answer and move on.  Hoda Time makes you decide, and gives you four ways to do it, from the most explicit to the most convenient:

[@fromCalendarDateTimeAll@] returns every valid mapping as a list: empty for a skipped time, one element in the ordinary case, and two (earlier then later) for an ambiguous one.  You look and choose.

[@fromCalendarDateTimeStrictly@] the cautious default — it succeeds with the single mapping, or fails in @MonadThrow@ with a @DateTimeDoesNotExistException@ for a skipped time or a @DateTimeAmbiguousException@ for an ambiguous one.  This is the resolver the opening example used.

[@fromCalendarDateTimeLeniently@] never fails: an ambiguous time collapses to the /earlier/ of its two instants, and a skipped time is nudged /forward/ by the length of the gap.

[@resolve@] you supply the policy.  It takes a handler for the ambiguous case (given both matches, earlier and later) and one for the skipped case (given the instant just before the gap and the one just after); @fromCalendarDateTimeStrictly@ and @fromCalendarDateTimeLeniently@ are themselves just @resolve@ with particular handlers.

The reverse journey never has this trouble: going from an @Instant@ to a @ZonedDateTime@ with @fromInstant@ (or @inTimeZone@), and back with @toInstant@, is always unambiguous — an instant is a genuine point on the timeline, and at any point a zone has exactly one offset in force.  The awkwardness is a property of civil labels, not of time itself.

=== Calendars

Every date type carries its calendar as a type parameter — @CalendarDate cal@, @CalendarDateTime cal@, @ZonedDateTime cal@.  The tag is a /phantom/: it selects the rules (the month names and lengths, the leap-year rule, the epoch) at no runtime cost, and, more importantly, it stops dates in different calendars from being mixed by accident — combining a Hebrew month with a Gregorian date is a compile error, not a lurking bug.  @Gregorian@ is the default, and the reference against which every other calendar is measured.

You construct dates through the module for the calendar you want, using /that/ calendar's own @calendarDate@ (plus @fromNthDay@ and @fromWeekDate@) and its own @Month@ and @DayOfWeek@ — @January@ for Gregorian, @Tishri@ for Hebrew, @Muharram@ for Islamic, and so on.  The full roster:

[Gregorian — "Data.HodaTime.Calendar.Gregorian"] The civil calendar used across most of the world today, and the timeline's reference point.  It is not proleptic: it begins at the 15 October 1582 changeover.

[ISO — "Data.HodaTime.Calendar.Iso"] Identical to Gregorian for every date; it differs only in week numbering — weeks start on Monday and week 1 is the first with at least four days in the new year.  Use its @fromWeekDate@ for ISO-8601 week dates.

[Julian — "Data.HodaTime.Calendar.Julian"] The \"Old Calendar\" that preceded the Gregorian and is still used liturgically by parts of the Eastern Orthodox church.  Fully proleptic with astronomical year numbering, floored at its introduction in 45 BC.

[Coptic — "Data.HodaTime.Calendar.Coptic"] The Coptic (Alexandrian) calendar: twelve thirty-day months followed by a short thirteenth.

[Persian — "Data.HodaTime.Calendar.Persian"] The astronomical Solar Hijri calendar, Iran's official civil calendar, whose year begins on the spring equinox as observed at Tehran.

[Islamic — "Data.HodaTime.Calendar.Islamic"] The tabular Islamic (Hijri) calendar, /parameterised by its leap-year pattern/ (see below).

[Hebrew — "Data.HodaTime.Calendar.Hebrew"] The Hebrew (Jewish) lunisolar calendar, /parameterised by its month numbering/ (see below).

__Parameterised calendars.__  A couple of calendars come in more than one variant, and rather than bury the choice in a runtime flag Hoda Time lifts it into the type too — just like the calendar itself.  @Islamic@ is tagged with its /leap pattern/ — @IslamicBase15@, @IslamicIndian@, @IslamicHabashAlHasib@, or @IslamicBcl@ (the .NET-compatible Base16 default) — which decides which years of the thirty-year cycle gain a day; @Hebrew@ is tagged with its /month numbering/ — @HebrewCivil@ (counting from Tishri, the default) or @HebrewScriptural@ (counting from Nisan).  Each variant is a distinct type, so a Base15 date can never be confused with a Base16 one, and the plain @calendarDate@ in each module still builds the default variant with no annotation required.

__Converting between calendars.__  Because they all share the one timeline, a single moment can be re-expressed in any of them with @withCalendar@ — there is a version at each level, in "Data.HodaTime.CalendarDate", "Data.HodaTime.CalendarDateTime" and "Data.HodaTime.ZonedDateTime" — which keeps the underlying day (or instant and time zone) and changes only the calendar the value is labelled in.  The target is chosen by the result type; the cookbook on "Data.HodaTime.CalendarDate" has a worked @withCalendar@ example.

=== Patterns

Turning these types into text, and text back into these types, is the job of a @Pattern@ (see "Data.HodaTime.Pattern").  Where most libraries hand you two separate stringly-typed operations — a format string in one direction and a parse string in the other — Hoda Time uses a /single/ @Pattern@ value that goes both ways.  A pattern is assembled from typed pieces rather than from a mini-language buried in a string, so a field that makes no sense for the type you are formatting is a compile error rather than a run-time surprise, and the very value that printed a date will read one back.

You drive a pattern with two functions: @format@ turns a value into a @String@, and @parse@ reads one back in any @MonadThrow@ — so @Maybe@, @IO@, @Either SomeException@ and friends all work — failing with a @ParseFailedException@ on bad input.  @parse'@ is the same but lets you supply the value whose fields fill in for anything the pattern does not mention.  (Worked @format@\/@parse@ examples are on "Data.HodaTime.Pattern".)

__The standard patterns.__  For the common layouts there is a ready-made pattern per type, each named for its Noda Time counterpart.  For a @CalendarDate@ (see "Data.HodaTime.Pattern.CalendarDate"):

[@pd@] short date, @dd\/MM\/yyyy@.

[@pD@] long date, @dddd, dd MMMM yyyy@.

[@pR@] the ISO-8601 round-trippable date, @yyyy-MM-dd@.

For a @LocalTime@ (see "Data.HodaTime.Pattern.LocalTime"):

[@pt@] short time, @HH:mm@.

[@pT@] long time, @HH:mm:ss@.

[@pr@] round-trippable time, @HH:mm:ss.fffffffff@, down to the nanosecond.

For a @CalendarDateTime@ (see "Data.HodaTime.Pattern.CalendarDateTime"), which simply glues a date pattern to a time pattern:

[@ps@] the sortable ISO form, @yyyy-MM-ddTHH:mm:ss@.

[@pf@ and @pF@] full: the long date with the short (@pf@) or long (@pF@) time.

[@pg@ and @pG@] general: the short date with the short (@pg@) or long (@pG@) time.

[@po@] the round-trippable form — @ps@ carried down to the nanosecond.

And the anchored and measured types have their own patterns:

[@pInstant@ — "Data.HodaTime.Pattern.Instant"] an @Instant@ as ISO-8601 UTC, @yyyy-MM-ddTHH:mm:ssZ@ (@pInstantNano@ carries the fraction).

[@pOffset@ — "Data.HodaTime.Pattern.Offset"] an @Offset@ as @(+\/-)HH:mm@ (@pOffsetFull@ adds seconds).

[@pOffsetDateTime@ — "Data.HodaTime.Pattern.OffsetDateTime"] an @OffsetDateTime@ as @yyyy-MM-ddTHH:mm:ss(+\/-)HH:mm@.

[@pDuration@ — "Data.HodaTime.Pattern.Duration"] a @Duration@ as @[-]D:HH:mm:ss@ (@pDurationNano@ carries the fraction).

[@pZonedDateTime@ — "Data.HodaTime.Pattern.ZonedDateTime"] a @ZonedDateTime@ as its local time followed by the zone id.  Formatting is pure; /parsing/ one is effectful (see below).

__Building your own.__  A standard pattern is nothing more than the field patterns from those same modules combined with two operators, and you assemble your own the same way: @\<\>@ merges two fields, and @\<%@ appends a fixed literal (built with @char@ or @string@).  The cookbook on "Data.HodaTime.Pattern" has a worked custom-pattern example.

The field patterns cover the usual components: @pyyyy@, @pMM@ (numeric month), @pMMM@ and @pMMMM@ (abbreviated and full month name) and @pdd@ (day), plus @pddd@ and @pdddd@ (abbreviated and full weekday name) for dates; @pHH@ (24-hour) or @phh@ (12-hour) with @pp@ \/ @ppp@ for the AM\/PM designator, then @pmm@, @pss@ and @pfrac@ for times.  @pfrac@ is the one pattern that takes an argument — the number of fractional-second digits, from 1 (tenths) up to 9 (nanoseconds) — because a single width covers every case cleanly.

__Locale-driven patterns.__  Beyond the fixed, English patterns, Hoda Time can read the /machine's own/ conventions from the operating system's locale database (see "Data.HodaTime.Locale").  @currentLocale@ (or @localeByName@) hands back a @Locale@, and "Data.HodaTime.Pattern.Locale" compiles that locale's short-date layout into an ordinary pattern with @localeDatePattern@ — so the same date prints @03\/15\/2020@ under @en_US@ but @15.03.2020@ under @de_DE@ — with @localeTimePattern@ doing the same for the time-of-day layout.  The name patterns have locale-aware variants too: @pMMMM'@, @pMMM'@, @pdddd'@, @pddd'@ and @ppp'@ each take a @Locale@ and use its month\/weekday names and AM\/PM designators in place of the built-in English ones.  This reads from the machine rather than bundling data (the same philosophy as the time-zone support), and for now is POSIX-only (Linux and macOS), covering the Gregorian names the OS exposes.

__Parsing a @ZonedDateTime@.__  Building a @ZonedDateTime@ has to load the zone rules and resolve the local time (which may be skipped or ambiguous), so it cannot come from the pure @parse@.  Instead @parseZonedDateTime@ takes a zone /provider/ (@timeZone@ in @IO@, or a pure lookup) and a /resolver/ (one of the four from the section on offsets and zones) and does it effectfully; formatting with @pZonedDateTime@ stays pure.

__What patterns do not yet do.__  This is a deliberately honest list; each item is on the roadmap rather than a decision against it.

* The /standard/ patterns are /fixed format/ — @pd@ is always @dd\/MM\/yyyy@, with English names taken from each calendar's own @Month@ and @DayOfWeek@.  The locale-driven patterns above now follow the machine's own layout and names, but only for the Gregorian names the OS exposes, only on POSIX (no Windows reader yet), and not yet for the combined date-and-time layout (@D_T_FMT@, which also carries a time zone).
* A weekday in a pattern (@pddd@ or @pdddd@) is /consumed but not validated/ on a parse: since the day, month and year already fix the date, the weekday is not checked against them.
* The abbreviated month @pMMM@ is just the first three letters of the name, which is ambiguous where two months share a prefix (the Hebrew @AdarI@ and @Adar@); use @pMMMM@ or @pMM@ when you need a guaranteed round-trip.
-}
module Data.HodaTime
(
)
where
