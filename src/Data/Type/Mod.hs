{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Type.Mod
  ( Mod
  , enum
  ) where

import Data.Type.Natural

data SingDict n where
  SingDict :: SingI n => Proxy n -> SingDict n

class IsSing f where
  singDict :: forall n. f n -> SingDict n

  withSingDict :: forall n a. f n ->(SingI n => f n -> a) ->  a
  withSingDict x f = case singDict x of
                       SingDict Proxy -> f x

instance IsSing SNat where
  singDict SZ = SingDict Proxy
  singDict (SS n) = case singDict n of
                      SingDict Proxy -> SingDict Proxy

data Mod (m :: Nat) where
  ModS :: Leq n m -> Mod ('S m)

toNat :: Leq n m -> SNat n
toNat (ZeroLeq{}) = SZ
toNat (SuccLeqSucc l) = SS $ toNat l

boundOf :: Leq a b -> SNat b
boundOf (ZeroLeq b) = b
boundOf (SuccLeqSucc b) = SS (boundOf b)

instance SingI m => Enum (Mod ('S m)) where
  toEnum n | n <= 0 = ModS $ ZeroLeq (sing :: SNat m)
           | otherwise = succMod $ toEnum $ n - 1
  fromEnum (ModS n) = sNatToInt $ toNat n

enum :: SNat n -> [Mod n]
enum SZ = []
enum (SS m) = withSingDict m $ \n -> [0 .. sNatToInt n]

instance SingI m => Show (Mod ('S m)) where
   show = show . fromEnum

instance SingI m => Ord (Mod ('S m)) where
  compare a b = compare (fromEnum a) (fromEnum b)

instance SingI m => Eq (Mod ('S m)) where
  a == b = fromEnum a == fromEnum b

testEq :: SNat n -> SNat m -> Maybe (Leq n m)
testEq SZ m = Just $ ZeroLeq m
testEq _ SZ = Nothing
testEq (SS n) (SS m) = fmap SuccLeqSucc $ testEq n m

succMod :: Mod b -> Mod b
succMod (ModS l) =
  case testEq (SS $ toNat l) b of
    Just f -> ModS f
    Nothing -> ModS $ ZeroLeq b
  where
    b = boundOf l

plusLeq :: SingI b => SNat a -> Mod b -> Mod b
plusLeq SZ m = m
plusLeq (SS b) m = plusLeq b (succMod m)

timesLeq :: (SingI b, Num (Mod b)) => SNat a -> Mod b -> Mod b
timesLeq SZ (ModS b) = ModS $ ZeroLeq $ boundOf b
timesLeq (SS b) m = m + timesLeq b m

instance SingI m => Num (Mod ('S m)) where
  ModS a + b = plusLeq (toNat a) b
  ModS a * b = timesLeq (toNat a) b
  abs m = m
  signum m = m
  fromInteger n
    | n > 0 = succMod (fromInteger $ n - 1)
    | otherwise = ModS (ZeroLeq (sing :: SNat m))
  _ - _ = error "unimplemented"

succLeq :: Leq n m -> Leq n ('S m)
succLeq (ZeroLeq n) = ZeroLeq (SS n)
succLeq (SuccLeqSucc l) = SuccLeqSucc $ succLeq l
