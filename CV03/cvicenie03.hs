module Cvicenie03 where

foldr' f z xs = foldl (flip f) z (reverse' xs)

reverse' xs = foldl (\ys -> \x ->  x:ys ) [] xs

scanr' f z xs = foldr (\x ys -> (f x (head ys):ys))[z] xs

data HTree = Leaf (Int, String) | Node HTree Int HTree deriving (Show, Eq)

instance Ord HTree where 
			t1 < t2 = weight t1 < weight t2
			t1 <= t2 = weight t1 <= weight t2
		--	t1 == t2 = weight t1 == weight t2

insertSort :: (Ord t) => t -> [t] -> [t]
insertSort a [] = [a]
insertSort a (x:xs) | a <= x = (a:x:xs)
					| otherwise = x:(insertSort a xs)

combine :: [HTree] -> [HTree]
combine (t1:t2:ts) = insertSort (Node t1 (weight t1 + weight t2) t2) ts

weight :: HTree -> Int
weight (Leaf (w,_)) = w
weight (Node _ w _) = w  

single xs = (length xs == 1)

huffman ft = head ((until single combine) ((map Leaf) ft))