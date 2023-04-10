module HW1.T2 (N (..), nplus, nmult, nsub, ncmp, nFromNatural, nToNum, nEven, nOdd, ndiv, nmod) where

import Data.Function
import GHC.Natural


data N = Z | S N


nplus :: N -> N -> N        -- addition
nplus n Z = n
nplus n (S m) = S (nplus n  m)


nmult :: N -> N -> N        -- multiplication
nmult n Z = Z
nmult Z n = Z
nmult n (S m) = nplus n (nmult n m)


nsub :: N -> N -> Maybe N   -- subtraction     (Nothing if result is negative)
nsub n Z = Just n
nsub Z (S _) = Nothing
nsub (S n) (S m) = nsub n m

ncmp :: N -> N -> Ordering  -- comparison      (Do not derive Ord)
ncmp n m = case (nsub n m) of
    Nothing -> LT
    Just Z -> EQ
    Just (S _) -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = 1 + (nToNum n)


nEven, nOdd :: N -> Bool    -- parity checking
nEven Z = True
nEven (S Z) = False
nEven (S (S n)) = nEven n

nOdd Z = False
nOdd (S Z) = True
nOdd (S (S n)) = nOdd n


fromJust :: Maybe a -> a
fromJust (Just a) = a


ndiv :: N -> N -> N         -- integer division
ndiv Z _ = Z
ndiv _ Z = Z
ndiv (S n) (S m) = if (ncmp (S n) (S m)) == LT then Z else (S (ndiv (fromJust (nsub (S n) (S m))) (S m)))


nmod :: N -> N -> N   -- modulo operation
nmod Z _ = Z
nmod (S n) (S m) = (fromJust (nsub (S n) (nmult (ndiv (S n) (S m)) (S m))))


