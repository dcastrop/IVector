{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module Data.Singletons.Dict
  ( Dict(..)
  , IsC(..)
  , withDict
  , withSingDict
  , withKnownNatDict
  ) where

import qualified Data.Singletons.TypeLits as TypeLits
import Data.Singletons.TypeLits hiding ( SNat )
import Data.Singletons

data Dict c (x :: k) where
  Dict :: c x => Proxy x -> Dict c x

class IsC c f where
  dict :: forall a. f a -> Dict c a

withDict :: forall c n a f. IsC c f => f n -> (c n => f n -> a) ->  a
withDict x f = case dict x :: Dict c n of
                 Dict Proxy -> f x

withSingDict :: forall n a f. IsC SingI f => f n -> (SingI n => f n -> a) ->  a
withSingDict = withDict @SingI

withKnownNatDict :: forall n a f. IsC KnownNat f => f n -> (KnownNat n => f n -> a) ->  a
withKnownNatDict = withDict @KnownNat

type SNat = (Sing :: Nat -> *)

instance IsC SingI SNat where
  dict TypeLits.SNat = Dict Proxy

instance IsC KnownNat SNat where
  dict TypeLits.SNat = Dict Proxy
