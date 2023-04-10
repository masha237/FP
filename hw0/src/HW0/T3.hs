module HW0.T3 (s, k, i, compose, contract, permute) where

s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g x = f x (g x)

k :: a -> b -> a
k x y = x


i :: a -> a
i = s k k


compose :: (b -> c) -> (a -> b) -> (a -> c)
-- | compose f g x = f g x
compose = s (k s) k

contract :: (a -> a -> b) -> (a -> b)
-- | contract f x = f x x
contract = s s (s k)

permute :: (a -> b -> c) -> (b -> a -> c)
permute = s ((s (k s) k) (s (k s) k) s)(k k)




