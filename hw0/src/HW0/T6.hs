module HW0.T6 () where
import HW0.T1 (distrib)

import Data.Char

a = distrib (Left ("AB" ++ "CD" ++ "EF"))     -- distrib from HW0.T1
b = map isSpace "Hello, World"
c = if 1 > 0 || error "X" then "Y" else "Z"

a_whnf = let x = Left ("AB" ++ "CD" ++ "EF") in (x, x)
b_whnf = isSpace 'H' : map isSpace "ello, World"
c_whnf = "Y"