module HW1.T3 (Tree(..),tsize,  tdepth, tFromList, tmember, tinsert, mkBranch)  where

import Data.Int

data Meta = Meta
            { height :: Int,
              size :: Int
            }

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)


getSize :: Tree a -> Int
getSize Leaf = 0
getSize (Branch meta l val r) = size meta

getHeight :: Tree a -> Int
getHeight Leaf = 0
getHeight (Branch meta l val r) = height meta

max' :: Int -> Int -> Int
max' a b = if a > b then a else b

getLeftSon :: Tree a -> Tree a
getLeftSon Leaf = Leaf
getLeftSon (Branch meta l a r) = l

getRightSon :: Tree a -> Tree a
getRightSon Leaf = Leaf
getRightSon (Branch meta l a r) = r

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch (Leaf) a (Leaf) = Branch Meta {height = 1, size = 1} Leaf a Leaf
mkBranch (Leaf) a (Branch meta l key r) = Branch Meta {height = 1 + (height meta), size = 1 + (size meta)} Leaf a (Branch meta l key r)
mkBranch (Branch meta l key r) a (Leaf) = Branch Meta {height = 1 + (height meta), size = 1 + (size meta)} (Branch meta l key r) a Leaf
mkBranch (Branch metaL lL keyL rL) a (Branch metaR lR keyR rR) = Branch Meta {height = 1 + (max' (height metaL) (height metaR)), size = 1 + (size metaL) + (size metaR)} (Branch metaL lL keyL rL) a (Branch metaR lR keyR rR)


updateRightSon :: Tree a -> Tree a -> Tree a
updateRightSon (Branch meta l key r) newR =
                mkBranch l key newR

updateLeftSon :: Tree a -> Tree a -> Tree a
updateLeftSon (Branch meta l key r) newL =
                mkBranch newL key r

makeRotate :: Tree a -> Tree a
makeRotate Leaf = Leaf
makeRotate (Branch meta l key r)
                | (getHeight l) - (getHeight r) == -2 = if ((getHeight (getLeftSon r)) - (getHeight (getRightSon r))) <= 0
                          then rotateLeft (Branch meta l key r) else  bigRotateLeft (Branch meta l key r)
                | (getHeight l) - (getHeight r) == 2 = if ((getHeight (getLeftSon l)) - (getHeight (getRightSon l))) >= 0
                          then rotateRight (Branch meta l key r) else  bigRotateRight (Branch meta l key r)
                | otherwise = (Branch meta l key r)

bigRotateLeft :: Tree a -> Tree a
bigRotateLeft Leaf = Leaf
bigRotateLeft (Branch meta l key r) =
        rotateLeft (updateRightSon (Branch meta l key r) (rotateRight r))

rotateLeft :: Tree a -> Tree a
rotateLeft Leaf = Leaf
rotateLeft (Branch meta l key r) =
         (updateLeftSon r (updateRightSon (Branch meta l key r) (getLeftSon r)))

bigRotateRight :: Tree a -> Tree a
bigRotateRight (Branch meta l key r) =
          rotateRight (updateLeftSon (Branch meta l key r) (rotateLeft l))

rotateRight :: Tree a -> Tree a
rotateRight (Branch meta l key r) =
          updateRightSon l (updateLeftSon (Branch meta l key r) (getRightSon l))


-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize a = getSize a


-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth a = getHeight a


-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember val Leaf = False
tmember val (Branch meta l key r)
         | val < key   = (tmember val l)
         | val == key  = True
         | otherwise   = (tmember val r)

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert val (Branch meta l key r)
         | val < key   = makeRotate
                              (updateLeftSon
                                  (Branch meta l key r)
                                  (tinsert val l)
                              )
         | val == key  = (Branch meta l key r)
         | otherwise   = makeRotate
                          (updateRightSon
                               (Branch meta l key r)
                               (tinsert val r)
                          )


-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList [] = Leaf
tFromList (x : xs) = tinsert x (tFromList xs)


tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ val Leaf = val

tfoldr f val (Branch meta l key r) = tfoldr f (f key (tfoldr f val r)) l


treeToList :: Tree a -> [a]    -- output list is sorted
treeToList = tfoldr (:) []
