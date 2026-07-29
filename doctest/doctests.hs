{-# LANGUAGE CPP #-}
module Main (main) where

import Test.DocTest (doctest)

-- | Run the doctest examples embedded in the library's Haddock documentation.
--
--   doctest re-interprets the sources, so it needs the library's own source
--   directories (including the platform-specific one) on the search path.  The
--   installed @hodatime@ package is hidden so that every module resolves to the
--   source we are checking, and the modules referenced from the @$setup@ block
--   are listed as targets so they load as home modules.
main :: IO ()
main = doctest
  [ "-isrc"
#if defined(mingw32_HOST_OS)
  , "-iplatform/windows"
#elif defined(darwin_HOST_OS)
  , "-iplatform/osx"
#else
  , "-iplatform/linux"
#endif
  -- Hide the installed hodatime package so its (hidden) internal modules do not
  -- shadow the sources we are loading from -isrc.
  , "-hide-package", "hodatime"
  , "src/Data/HodaTime/Internal/Lens.hs"
  , "src/Data/HodaTime/Calendar/Gregorian.hs"
  , "src/Data/HodaTime/CalendarDateTime/Internal.hs"
  ]
