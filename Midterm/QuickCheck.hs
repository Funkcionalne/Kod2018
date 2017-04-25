module QCH where

import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)

           
-- map f . drop n = drop n . map f
drop_map_f = 
  quickCheck(
            (\n -> \xs -> \f -> ((drop n) . (map f)) xs == ((map f) . (drop n)) xs)
            ::Int->[Int]->(Int->Int)->Bool)
            
-- take n . filter p = filter p . take n            
filter_take_f = 
  quickCheck(
            (\n -> \xs -> \p -> ((take n) . (filter p)) xs == ((filter p) . (take n)) xs)
            ::Int->[Int]->(Int->Bool)->Bool)
            
-- map f . concat = concat . map (map f)
concat_map_f = 
  quickCheck(
            (\xs -> \f -> ((map f) . concat) xs == (concat . (map (map f))) xs)
            ::[[Int]]->(Int->Int)->Bool)
            
-- concat . map f = foldr (\x -> \y -> f x ++ y) []
concat_map__foldr_f = 
  quickCheck(
            (\xs -> \f -> (concat . (map f)) xs == (foldr (\x -> \y -> f x ++ y) []) xs)
            ::[Int]->(Int->[Int])->Bool)
            