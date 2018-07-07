module Codewars.Kata.DecodeMorse (decodeMorse) where

import Codewars.Kata.DecodeMorse.Preload (morseCodes)
import Data.Map.Strict ((!))
import Data.List
import Data.List.Split

decodeMorse :: String -> String
decodeMorse = unwords . filter (/= "") . map (\str -> foldr (\x y -> morseCodes ! x ++ y) "" $ words str) . splitOn "   "
