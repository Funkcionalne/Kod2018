module Zloz where

import Data.List (permutations)

zloz :: [Int -> Int] -> Int ->  [Int -> Int] 
zloz fs x = result where
    tmp = (filter (\f -> calc f x /= 0) (permutations fs))
    result | length tmp == 0 = []
           | otherwise = head tmp

calc :: [Int -> Int] -> Int -> Int
calc fs n = (foldr (.) (\_ -> n) fs) n

{-
zloz [f1, f2]  0  == []
zloz [f1, f2, f3] 1 == [f2, f3, f1] 
-}

f1 x = if x == 1 then 1 else 0
f2 x = if x == 2 then 1 else 0
f3 x = if x == 1 then 2 else 0