module CanYouGetTheLoop where
import CanYouGetTheLoop.Types
import qualified Data.List as List

{-
data Node a
instance Eq a => Eq (Node a)

next :: Node a -> Node a
-}

_extract (Just a) = a

_loopSize ns n
  | index /= Nothing = 1 + _extract index
  | otherwise = _loopSize (n:ns) (next n)
  where index = List.elemIndex n ns

loopSize :: Eq a => Node a -> Int
loopSize n = _loopSize [] n
