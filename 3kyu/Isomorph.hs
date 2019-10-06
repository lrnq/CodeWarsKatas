module ISO where

import Data.Void
import qualified Data.Maybe as M
import Data.Tuple 

type ISO a b = (a -> b, b -> a)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

refl :: ISO a a
refl = (id, id)

symm :: ISO a b -> ISO b a
symm = swap 

trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = 
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (map ab, map ba)  

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (isoM ab, isoM ba)
  where
    isoM xy (Just x) = Just $ xy x
    isoM _ Nothing = Nothing

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (isoE ab cd, isoE ba dc)
  where
    isoE xyl _ (Left x) = (Left $ xyl x)
    isoE _ xyr (Right x) = (Right $ xyr x)
    

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (\ac -> cd . ac . ba, \bd -> dc . bd . ab)

isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (mab, mba) = (unMaybe mab, unMaybe mba)
  where
    unMaybe mxy x =  M.fromMaybe (M.fromJust $ mxy Nothing) (mxy $ Just x)

isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (ev, ve)
  where
    ev (Left x) = Left (():x)
    ev (Right ()) = Left []
    
    ve (Left []) = Right ()
    ve (Left (():x)) = Left x
    ve (Right x) = Right (absurd x)

isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (swap, swap)
