{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Data.Type.Vector
  ( Vec
  , pattern VNil
  , pattern VCons
  , vec
  , len
  , proj
  ) where

import Data.Type.Mod
import Data.Type.Natural
import Data.Typeable

newtype Vec (n :: Nat) a = Vec [a]

instance Show (Vec 'Z a) where
  show _ = "[]"

deriving instance Typeable n => Typeable (Vec n)

instance Functor (Vec n) where
  fmap f (Vec xs) = Vec (fmap f xs)

vnil :: Vec 'Z a
vnil = Vec []

vcons :: a -> Vec n a -> Vec ('S n) a
vcons x (Vec xs) = Vec (x : xs)

outV :: Vec ('S n) a -> (a, Vec n a)
outV (Vec (x : xs)) = (x, Vec xs)

{-# COMPLETE VNil, VCons #-}
pattern VNil :: Vec 'Z a
pattern VCons :: a -> Vec n a -> Vec ('S n) a

pattern VNil = Vec []
pattern VCons x xs <- (outV -> (x, xs)) where
  VCons x xs = vcons x xs

vec :: forall n a b. SNat n -> (Mod n -> a -> b) -> a -> Vec n b
vec n f x = Vec $ mapLL [] $ enum n
  where
    mapLL :: [b] -> [Mod n] -> [b]
    mapLL acc [] = acc
    mapLL acc (l : r) = mapLL (f l x : acc) r

len :: forall n a. SingI n => Vec n a -> SNat n
len _ = sing

proj :: forall m a. SingI m => Mod ('S m) -> Vec ('S m) a -> a
proj n (Vec l) = l !! fromEnum n
