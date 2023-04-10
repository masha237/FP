module HW1.T6 (mcat, epart) where

delJust :: Monoid a => Maybe a -> a
delJust (Just a) = a
delJust Nothing = mempty


mcat :: Monoid a => [Maybe a] -> a
mcat list = foldl f mempty list where
            f :: Monoid a => a -> Maybe a -> a
            f accum x  = accum <> (delJust x)


epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart list = foldl f (mempty, mempty) list where
                         f :: (Monoid a, Monoid b) => (a, b) -> Either a b -> (a, b)
                         f (accumA, accumB) (Left a) = (accumA <> a, accumB)
                         f (accumA, accumB) (Right b) = (accumA, accumB <> b)

