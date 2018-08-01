{-# LANGUAGE LambdaCase #-}
module Coroutine where

import Control.Monad (ap, forever)
import Preloaded
import Debug.Trace

-- Preloaded contains the following:
-- {-# LANGUAGE DeriveFunctor #-}
--
-- newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)
--
-- data Command r u d a =
--   Done a
-- | Out d (Coroutine r u d a)
-- | In (u -> Coroutine r u d a) deriving Functor

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

cr :: Command r u d a -> Coroutine r u d a
cr com = Coroutine (\k -> k $ com)

instance Applicative (Coroutine r u d) where
  pure = return
  (<*>) = ap

instance Monad (Coroutine r u d) where
  return x = Coroutine (\k -> k $ Done x)
  f >>= g  = Coroutine (\k -> apply f (\(Done a) -> apply (g a) k ))

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
p1 >>> p2 = Coroutine (\k ->
  apply p2 
    (\case
      In i -> apply p1 (\case
                           Out o p1' -> apply (p1' >>> (i o)) k
                           Done t -> k $ Done t
                           In i' -> k $ In (\i'' -> (i' i'') >>> p2))
      Done t -> k $ Done t
      Out t1 t2 -> k $ Out t1 $ Coroutine (\k' -> apply (p1 >>> t2) k') ))

-- It might be useful to define the following function
-- pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a

-- Library functions

output :: a -> Coroutine r u a ()
output v = Coroutine (\k -> k $ Out v $ return ()) 

input :: Coroutine r v d v
input = Coroutine (\k -> k $ In (\v -> return v))

produce :: [a] -> Coroutine r u a ()
produce (x:xs) = Coroutine (\k -> k $ Out x $ produce xs)
produce [] = return ()

consume :: Coroutine [t] u t a -> [t]
consume c = apply c (\case
                      Out o c' -> (o:consume c')
                      Done _ -> [])
                      

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = Coroutine (\k -> k $ In (\v -> filterC' v))
  where
    filterC' x | p x  = Coroutine (\k -> k $ Out x $ filterC p)
               | otherwise = filterC p

limit :: Show v => Int -> Coroutine r v v ()
limit n 
  | n <= 0 = return ()
  | otherwise = cr $ In (\v -> limit' v)
  where
    limit' v = cr $ Out v $ limit $ n-1

suppress :: Int -> Coroutine r v v ()
suppress n 
  | n <= 0 = filterC (\_ -> True)
  | otherwise = cr $ In (\_ -> suppress $ n-1)

add :: Coroutine r Int Int ()
add = cr $ In (\v1 -> cr $ In (\v2 -> cr $ Out (v1 + v2) add))

duplicate :: Coroutine r v v ()
duplicate = cr $ In (\v -> cr $ Out v $ cr $ Out v duplicate)

mapC :: (a -> b) -> Coroutine r a b ()
mapC f = cr $ In (\v -> cr $ Out (f v) $ mapC f) 

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers 
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = filterC even >>> limit 5
p2 = produce [1..] >>> mapC (\v -> v * (v + 1) `div` 2)
p3 = duplicate >>> add
p4 = duplicate >>> suppress 1 >>> add
