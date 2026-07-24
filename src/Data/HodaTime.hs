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

== Cookbook

=== USA Holidays

>>>import Data.HodaTime.CalendarDate (DayNth(..))
>>>import Data.HodaTime.Calendar.Gregorian (calendarDate, fromNthDay, Month(..), DayOfWeek(..), Gregorian)

>>>usaHolidays y = catMaybes $ ($ y) <$>
        [
           calendarDate 1 January               -- New Year
          ,calendarDate 4 July                  -- Independence Day 
          ,calendarDate 25 December             -- Christmas
          ,fromNthDay First Monday September    -- Labor day
          ,fromNthDay Third Monday January      -- MLK day
          ,fromNthDay Second Tuesday February   -- Presidents day
          ,fromNthDay Fourth Thursday November  -- Thanksgiving
          ,calendarDate 29 February             -- Leap day (not a real holiday but demonstrates date that may not exist)
        ]
-}
module Data.HodaTime
(
)
where
