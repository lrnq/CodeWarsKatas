{-# LANGUAGE GADTs #-}
module Combinators where
import PredefinedCombinators (SKI(..))


-- Task #1: Read SKI's data type (pre)defined as below, and understand what's going on ;-)

{-
 data SKI :: * -> * where
   Ap :: SKI (a -> b) -> SKI a -> SKI b
   S :: SKI ((a -> b -> c) -> (a -> b) -> a -> c)
   K :: SKI (a -> b -> a)
   I :: SKI (a -> a)
-}

-- Task #2: implement the evaluator and pretty-printer for the SKI system.

evalSKI :: SKI a -> a
evalSKI (Ap f x) = (evalSKI f) (evalSKI x)
evalSKI S = \x y z -> (x z) (y z)
evalSKI K = const
evalSKI I = id

-- The pretty-printer should follow this format:
-- I, K, S -> "I", "K", "S"
-- Ap a b -> "(a b)" where a and b are subterms
prettyPrintSKI :: SKI a -> String
prettyPrintSKI (Ap f x) = "(" ++ prettyPrintSKI f ++ " " ++ prettyPrintSKI x ++ ")"
prettyPrintSKI S = "S"
prettyPrintSKI K = "K"
prettyPrintSKI I = "I"


------------------------------------------------------------------------------

-- \x.\y.yx
-- \x.S I (K x)
-- S (K (S I)) K
rev :: SKI (a -> (a -> b) -> b)
rev = Ap (Ap S (Ap K (Ap S I))) K

-- \x.\y.\z.x(yz)
-- \x.\y.S (K x) y
-- \x.S (\y. S (K x)) I
-- \x.S (S (K S) (\y.Kx)) I
-- \x.S (S (K S) (K (K x))) I
-- S (\x.S (S (K S) (K (K x)))) (K I)
-- S (S (\x.S (S (K S)) \x.(K (K x))) (K I)
-- S (S (K (S (S (K S)))) (S (K K) K)) (K I)
comp :: SKI ((b -> c) -> (a -> b) -> (a -> c))
comp = Ap (Ap S (Ap (Ap S (Ap K (Ap S (Ap S (Ap K S))))) (Ap (Ap S (Ap K K)) K))) (Ap K I)

-- \x.\y.\z->xzy
-- \x.\y.S x (K y)
-- \x.S (\y. S x) K
-- \x.S (S (K S) (K x)) K
-- S (\x.S (S (K S) (K x))) (K K)
-- S (S (K S) (\x.S (K S) (K x))) (K K)
-- S (S (K S) (S (K (S (K S))) K)) (K K)
flip' :: SKI ((a -> b -> c) -> (b -> a -> c))
flip' = Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap K (Ap S (Ap K S)))) K))) (Ap K K)

-- \x.\y.\z.yzx
-- \x.\y.S y (K x)
-- \x.S S (K (K x))
-- S (K (S S)) (\x.K (K x))
-- S (K (S S)) (S (K K) K)
rotr :: SKI (a -> (c -> a -> b) -> c -> b)
rotr = Ap (Ap S (Ap K (Ap S S))) (Ap (Ap S (Ap K K)) K)

-- \x.\y.\z.zxy
-- \x.\y.S (\z.zx) (K y)
-- \x.\y.S (S I (K x)) (K y)
-- \x.S (K (S (S I (K x)))) K
-- S (\x.S (K (S (S I (K x))))) (K K)
-- S (S (K S) (\x.K (S (S I (K x))))) (K K)
-- S (S (K S) (S (K K) (\x.S (S I (K x))))) (K K)
-- S (S (K S) (S (K K) (S (K S) (\x.S I (K x))))) (K K)
-- S (S (K S) (S (K K) (S (K S) (S (K (S I)) K)))) (K K)
rotv :: SKI (a -> b -> (a -> b -> c) -> c)
rotv = Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap K K)) (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap K (Ap S I))) K))))) (Ap K K)

-- We can't write `fix` i.e Y in Haskell because Haskell is typed
-- (well, at least without recursive types), but we can still write `join`
-- \x.\y.xyy
-- \x.S x I
-- S S (K I)
join :: SKI ((a -> a -> b) -> a -> b)
join = Ap (Ap S S) (Ap K I)

------------------------------------------------------------------------------
-- Task #4: implement Boolean algebra in the SKI system
type Bool' a = a -> a -> a

true :: SKI (Bool' a)
true = K

false :: SKI (Bool' a)
false = Ap K I

not' = Ap (Ap S (Ap (Ap S I) (Ap K false))) (Ap K true)

and' = Ap (Ap S S) (Ap K (Ap K false))

or' = Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap K K)) (Ap (Ap S I) (Ap K true))))) (Ap K I)

xor' = Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap (Ap S (Ap K S)) K)) (Ap K (Ap (Ap S (Ap (Ap S I) (Ap K (Ap K I)))) (Ap K K)))))) (Ap (Ap S (Ap (Ap S (Ap K S)) (Ap K (Ap (Ap S I) (Ap K K))))) (Ap K (Ap K (Ap K I))))
