{-# LANGUAGE RankNTypes #-}

module Data.HodaTime.Internal.Lens
(
   Lens
  ,view
  ,set
  ,modify
)
where

import Control.Applicative

-- This module is copied almost word for word from the basic-lens package.  We would use that instead but it's not supported by this version of stack lts
-- which means we have to use stack only features to get it to work.  So for now, we just make our own version.

-- TODO: Get rid of this and use the package when we upgrade to where it is supported

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

newtype Id a = Id { runId :: a }

instance Functor Id where fmap f = Id . f . runId

view :: Lens s t a b -> s -> a
view l = getConst . l Const

modify :: Lens s t a b -> (a -> b) -> s -> t
modify l f = runId . l (Id . f)

set :: Lens s t a b -> b -> s -> t
set l a = runId . l (Id . const a)