{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Kata.AdditionCommutes
  ( plusCommutes ) where

import Kata.AdditionCommutes.Definitions
  ( Z, S
  , Natural(..), Equal(..)
  , (:+:))

reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS a) = EqlS (symmetric a)

transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS a) (EqlS b) = EqlS (transitive a b)

rightS :: Natural a -> Natural b -> Equal (S (a :+: b)) (a :+: S b)
rightS NumZ b = EqlS (reflexive b)
rightS (NumS a) b = EqlS (rightS a b)

plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ = EqlZ
plusCommutes NumZ (NumS b) = EqlS (plusCommutes NumZ b)
plusCommutes (NumS a) b = transitive (EqlS (plusCommutes a b)) (rightS b a)
