-- INF 1 Functional Programming
-- 
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT                  
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1+ max (depth left )(depth right) 

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k a left right) = toList left ++ [(k,a)]++toList right 
-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get k Leaf = Nothing
get k (Node x y left right)|k==x =Just y
                           |k <= x  = get k left
                           |otherwise = get k right 

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList xs = foldr (\x acc -> set (fst x) (snd x) acc) Leaf xs 


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT k  Leaf  = Leaf
filterLT k (Node x y left right) = fromList (filter (\x-> fst x <k) $ toList (Node x y left right))

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT k Leaf=Leaf
filterGT k (Node x y left right) = fromList (filter (\x-> fst x >k) $ toList (Node x y left right))


-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf Leaf=Leaf
merge Leaf (Node x y left right) =Node x y left right
merge (Node x y left right) Leaf =Node x y left right
merge (Node x y l1 r1 ) (Node m n l2 r2)=   Node m n (merge (filterLT m (Node x y l1 r1)) l2) (merge (filterGT m (Node x y l1 r1)) r2)
--prop_merge :: Ord k => Keymap k a -> Bool
--prop_merge =
--prop_merge =
--merge (Node x y l1 r1 ) (Node m n l2 r2)=map (\a-> set  (fst a) (snd a) (Node x y l1 r1)) (toList (Node m n l2 r2)))
-- foldr (set (fst )(snd )) (Node x y l1 r1) (toList (Node m n l2 r2))    
-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del d Leaf=Leaf
del d (Node x y l r) = f (merge (Node x y l r)(Node d y Leaf Leaf))
                     where f (Node m n l r)= merge l r  

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select f Leaf =Leaf  
select f (Node x y l r) = fromList (filter (\x -> (f.snd) x)(toList (Node x y l r)))
-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
