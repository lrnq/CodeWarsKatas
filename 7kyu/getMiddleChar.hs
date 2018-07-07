module Codewars.G964.Getmiddle where

getMiddle :: String -> String
getMiddle s
    |odd $ length s = take (1) $ drop (middle s) $ s
    |even $ length s = take (2) $ drop (middle s-1) $ s
    where middle s = div (length s) (2)
