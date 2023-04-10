module HW0.T2 (doubleNeg, reduceTripleNeg, Not) where

import Data.Void
type Not a = a -> Void


-- | a->Not(a->Void)
-- | a->(a->Void)->Void
doubleNeg :: a -> Not (Not a)
doubleNeg a f = f a



-- | (((a->Void)->Void)->Void)->(a->Void)
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg f a = f (doubleNeg a)