module HW1.T7 (ListPlus (..), Inclusive (..), DotString (..), Fun (..)) where

data ListPlus a = a :+ ListPlus a | Last a
        deriving (Show)
infixr 5 :+

instance Semigroup (ListPlus a) where
    Last a <> b = a :+ b
    (head :+ tail) <> b = head :+ (tail <> b)




data Inclusive a b = This a | That b | Both a b deriving (Show)

instance (Semigroup a, Semigroup b)  => Semigroup (Inclusive a b) where
    This a <> This b = This (a <> b)
    This a <> That b = Both a b
    This a <> Both ar br = Both (a <> ar) br

    That a <> This b = Both b a
    That a <> That b = That (a <> b)
    That a <> Both ar br = Both ar (a <> br)

    Both al  ar <> This b = Both (al <> b) ar
    Both al  ar <> That b = Both al (ar <> b)
    Both al  ar <> Both bl br = Both (al <> bl) (ar <> br)



newtype DotString = DS String deriving (Show)
instance Monoid DotString where
    mempty             =  DS ""
instance Semigroup DotString where
    (DS "") <> (DS l2) = DS l2
    (DS l1) <> (DS "") = DS l1
    (DS l1) <> (DS l2) = DS (l1 ++ "." ++ l2)


newtype Fun a = F (a -> a)
instance Monoid (Fun a) where
  mempty = F id
instance Semigroup (Fun a) where
   F a <> F b = F (a . b)

