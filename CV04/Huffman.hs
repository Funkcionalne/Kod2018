module Huffman where

data HTree = Leaf (Int, String) | Node HTree Int HTree deriving (Show, Eq)

instance Ord HTree where 
			t1 < t2 = weight t1 < weight t2
			t1 <= t2 = weight t1 <= weight t2
		--	t1 == t2 = weight t1 == weight t2

weight :: HTree -> Int
weight (Leaf (n, s)) = n
weight (Node l n r) = n       

insert :: (Ord t) => t -> [t] -> [t]
insert a [] = [a]
insert a ys@(x:xs) = if a < x then (a:ys) else x:(insert a xs)


combine :: [HTree] -> [HTree]
combine (x:y:xs) = insert temp xs
  where temp = (Node x (weight x + weight y) y) 

isSingle :: [HTree] -> Bool
isSingle xs = length xs == 1

huffman :: [(Int, String)] -> HTree
huffman ft = head $ until isSingle combine (map Leaf ft)