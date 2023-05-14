{-|
Module      :  Data.HodaTime.Interval
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

<snip - add stuff rest of documentation>

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
