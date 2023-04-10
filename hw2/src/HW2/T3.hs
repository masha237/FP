module HW2.T3 where

import HW2.T1 (Option (..),
               Pair (..),
               Quad (..),
               Annotated (..),
               Except (..),
               Stream (..),
               List (..),
               Fun (..),
               Prioritised (..))

joinOption    :: Option (Option a) -> Option a
joinOption (None) = None
joinOption (Some None) = None
joinOption (Some (Some a)) = Some a

joinExcept    :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e
joinExcept (Success (Error e)) = Error e
joinExcept (Success (Success a)) = Success a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e2) :# e1) = a :# e1 <> e2

joinList      :: List (List a) -> List a
joinList Nil = Nil
joinList (Nil :. tail) = joinList tail
joinList ((a :. list) :. tail) = a :. (joinList (list :. tail))



joinFun       :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\out -> case f out of (F inF) -> inF out)