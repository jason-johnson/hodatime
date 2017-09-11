module HodaTime.Util
(
   get
  ,modify
  ,set
)
where

import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..))

get :: ((s -> Const s c) -> a -> Const t b) -> a -> t
get l = getConst . l Const
  
modify :: (s -> b) -> ((s -> Identity b) -> a -> Identity t) -> a -> t
modify f l = runIdentity . l (Identity . f)
  
set :: s -> ((b -> Identity s) -> a -> Identity t) -> a -> t
set v = modify (const v)