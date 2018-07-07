module Codewars.G964.Potatoes where

potatoes :: Int -> Int -> Int -> Int
potatoes p0 w0 p1 = round $ fromIntegral $ (w0 * (100 - p0) `div` (100 - p1))
