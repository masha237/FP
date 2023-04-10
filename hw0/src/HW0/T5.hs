module HW0.T5 (Nat, nz, ns, nplus, nmult, nFromNatural, nToNum) where

import Data.Function
import GHC.Natural
type Nat a = (a -> a) -> a -> a



nz :: Nat a
nz f x = x
ns :: Nat a -> Nat a
ns n = \f x -> f (n f x)

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus n m = \f x -> m f (n f x)
nmult n m = \f x -> m (n f) x

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns (nFromNatural (n - 1))


nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0











