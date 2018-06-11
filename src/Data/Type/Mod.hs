{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Type.Mod
  ( TMod
  , enum
  , modVal
  , extendMod
  ) where

import Data.Singletons
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Num
import Data.Singletons.TypeLits
import Data.Type.Equality

import Unsafe.Coerce

-- FIXME: Unsafe coerce for 'less-than' constraints!
data TMod (n :: Nat)
  = forall m. (KnownNat n, KnownNat m) => TMod (SNat m) ((m < n) :~: 'True)

plusMod :: forall n. TMod n -> TMod n -> TMod n
plusMod (TMod (a :: SNat a) _) (TMod (b :: SNat b) _)
  = withKnownNat ab $ TMod ab (unsafeCoerce Refl)
  where
    ab :: SNat (a + b `Mod` n)
    ab = (a %+ b) `sMod` (sing :: SNat n)

multMod :: forall n. TMod n -> TMod n -> TMod n
multMod (TMod (a :: SNat a) _) (TMod (b :: SNat b) _)
  = withKnownNat ab $ TMod ab (unsafeCoerce Refl)
  where
    ab :: SNat (a + b `Mod` n)
    ab = (a %+ b) `sMod` (sing :: SNat n)

negMod :: forall n. TMod n -> TMod n
negMod (TMod a _) = withKnownNat na $ TMod na (unsafeCoerce Refl)
  where
    na = (sing :: SNat n) %- a

minusMod :: forall n. TMod n -> TMod n -> TMod n
minusMod a b = plusMod a (negMod b)

prodMod :: forall n. TMod n -> TMod n -> TMod n
prodMod (TMod a _) (TMod b _) = withKnownNat ab $ TMod ab (unsafeCoerce Refl)
  where
    ab = a %* b

modVal :: TMod n -> SomeSing Nat
modVal (TMod n _) = SomeSing n

extendMod :: forall m n. (KnownNat n, KnownNat m) => TMod n -> TMod (m + n)
extendMod (TMod a _) = withKnownNat mn $ TMod a (unsafeCoerce Refl)
  where
    mn = (sing :: SNat m) %+ (sing :: SNat n)

instance KnownNat n => Num (TMod n) where
  (+) = plusMod
  (-) = minusMod
  negate = negMod
  abs m = m
  signum (TMod m p) = (TMod (SNat :: SNat 1) (unsafeCoerce Refl))
  fromInteger i
    = case toSing $ fromIntegral (i `mod` (fromIntegral $ fromSing (sing :: SNat
    n))) of
        SomeSing x@SNat -> TMod x (unsafeCoerce Refl)
  (*) = prodMod

instance KnownNat n => Enum (TMod n) where
  toEnum i = fromInteger $ fromIntegral i
  fromEnum (TMod n _) = fromIntegral $ natVal n

instance Show (TMod n) where
  show (TMod n _) = show $ fromSing n

enum :: SNat n -> [TMod n]
enum n = withKnownNat n $ map fromInteger [0..i-1]
  where
    i = withKnownNat n $ fromIntegral $ natVal n
