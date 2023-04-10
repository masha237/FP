module HW1.T4 (tfoldr, treeToList) where

import HW1.T3 (Tree (..), mkBranch)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ val Leaf = val

tfoldr f val (Branch meta l key r) = tfoldr f (f key (tfoldr f val r)) l


treeToList :: Tree a -> [a]    -- output list is sorted
treeToList = tfoldr (:) []