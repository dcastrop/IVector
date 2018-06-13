{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Data.Type.Vector
  ( Vec
  , pattern VNil
  , pattern VCons
  , vec
  , len
  , proj
  , enum
  , foldr
  ) where

import Prelude hiding ( foldr )
import qualified Prelude

import Data.Type.Mod hiding ( enum )
import qualified Data.Type.Mod as Mod
import Data.Singletons.Prelude
import Data.Singletons.TypeLits

import Data.Typeable

newtype Vec (n :: Nat) a = Vec [a]

instance Show a => Show (Vec n a) where
  show (Vec xs) = show xs

deriving instance Typeable n => Typeable (Vec n)

instance Functor (Vec n) where
  fmap f (Vec xs) = Vec (fmap f xs)

vnil :: Vec 0 a
vnil = Vec []

vcons :: a -> Vec n a -> Vec (1 + n) a
vcons x (Vec xs) = Vec (x : xs)

outV :: Vec (1+n) a -> (a, Vec n a)
outV (Vec (x : xs)) = (x, Vec xs)

{-# COMPLETE VNil, VCons #-}
pattern VNil :: Vec 0 a
pattern VCons :: a -> Vec n a -> Vec (1 + n) a

pattern VNil = Vec []
pattern VCons x xs <- (outV -> (x, xs)) where
  VCons x xs = vcons x xs

vec :: forall n a b. KnownNat n => (TMod n -> a -> b) -> a -> Vec n b
vec f x = Vec $ mapLL [] $ Mod.enum n
  where
    n = sing :: SNat n
    mapLL :: [b] -> [TMod n] -> [b]
    mapLL acc [] = reverse acc
    mapLL acc (l : r) = mapLL (f l x : acc) r

len :: forall n a. SingI n => Vec n a -> SNat n
len _ = sing

proj :: forall m a. KnownNat m => TMod m -> Vec m a -> a
proj n (Vec l) = l !! fromEnum n

enum :: forall m. SNat m -> Vec m (TMod m)
enum n = Vec xs
  where
    xs = Mod.enum n

foldr :: (a -> b -> b) -> b -> Vec x a -> b
foldr f x (Vec xs) = Prelude.foldr f x xs
