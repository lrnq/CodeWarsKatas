module Codewars.Kata.XO where
import Data.Char (toLower)

xo :: String -> Bool
xo str = os == xs
  where os = sum [1 | s <- str, toLower s == 'o']
        xs = sum [1 | s <- str, toLower s == 'x']
