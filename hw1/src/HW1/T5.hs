module HW1.T5 (splitOn, joinWith) where

import Data.List.NonEmpty


splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = [] :| []
splitOn sep (x: xs) = if (sep == x) then [] <| ans else (x : head) :| tail where ans@(head :| tail) = splitOn sep xs




joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (x :| []) = x
joinWith sep (x :| y : xs) = x ++ [sep] ++ (joinWith sep (y :| xs))







