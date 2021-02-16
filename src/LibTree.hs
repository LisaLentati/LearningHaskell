module LibTree where

import Test.QuickCheck (quickCheck)

data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving Show

flatten :: Tree a -> [a]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

treeA = Node 
            (Node (Leaf 'a') 'b' (Leaf 'c')) 
            'd' 
            (Node (Leaf 'e') 'f' (Leaf 'g'))

is_search_tree :: (Ord a) => Tree a -> Bool
is_search_tree tree = is_list_sorted (flatten tree) 


is_list_sorted :: (Ord a) => [a] -> Bool
is_list_sorted list = if length list < 2 
    then True
    else (x <= y) && (is_list_sorted xs) where      
        x:xs = list
        y:_= xs

