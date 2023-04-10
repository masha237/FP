module HW2.T1 where

data Option a = None | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e
infix 0 :#

data Except e a = Error e | Success a

data Prioritised a = Low a | Medium a | High a

data Stream a = a :> Stream a
infixr 5 :>

data List a = Nil | a :. List a
infixr 5 :.

data Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)


mapOption      :: (a -> b) -> (Option a -> Option b)
mapOption f None = None
mapOption f (Some a) = Some (f a)


mapPair        :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a b) = P (f a) (f b)

mapQuad        :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a b c d) = Q (f a) (f b) (f c) (f d)


mapAnnotated   :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = (f a) :# e

mapExcept      :: (a -> b) -> (Except e a -> Except e b)
mapExcept f (Error e) = Error e
mapExcept f (Success a) = Success (f a)


mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a) = Low (f a)
mapPrioritised f (Medium a) = Medium (f a)
mapPrioritised f (High a) = High (f a)

mapStream      :: (a -> b) -> (Stream a -> Stream b)
mapStream f (a :> stream) =  (f a) :> (mapStream f stream)


mapList        :: (a -> b) -> (List a -> List b)
mapList f Nil = Nil
mapList f (a :. tail) = (f a) :. (mapList f tail)

mapFun         :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F fun) = F (f . fun)

mapTree        :: (a -> b) -> (Tree a -> Tree b)
mapTree f (Leaf) = Leaf
mapTree f (Branch l v r) = Branch (mapTree f l) (f v) (mapTree f r)
