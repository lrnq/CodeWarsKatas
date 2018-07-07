module Kata where

findMissingLetter :: [Char] -> Char
findMissingLetter cs = head [x | x <- [(head cs)..(last cs)], x `notElem` cs]
