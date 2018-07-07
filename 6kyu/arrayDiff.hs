module Difference where

difference :: Eq a => [a] -> [a] -> [a]
difference = foldl diff          
             where diff xs y = filter (/=y) xs
