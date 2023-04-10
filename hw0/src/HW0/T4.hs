module HW0.T4 (repeat', map', fib, fac) where

import Data.Function
import GHC.Natural

repeat' :: a -> [a]             -- behaves like Data.List.repeat
repeat' a = fix (a: )


map' :: (a -> b) -> [a] -> [b]  -- behaves like Data.List.map
map' f = fix $ \rec list -> case list of
          [] -> []
          (x: xs) -> f x : rec xs

fib' _ 0 x _ = x
fib' _ 1 _ y = y
fib' f n x y = f (n - 1) y (x + y)

fib :: Natural -> Natural       -- computes the n-th Fibonacci number
fib n = fix fib' n 0 1




fac :: Natural -> Natural       -- computes the factorial
fac = fix $ \rec d -> if d < 2 then 1 else d * rec (d - 1)



