module Huffman where

import FreqTable
import Data.Char (toLower)

-- silno inspirovane knihou R.Bird, P.Wadler: Introduction to Functional Programming
-- pdf na webe
-- citajte kap. 9.2 Huffman coding trees

type Weight = Int
type Pair = (Weight, String)
data HTree = Leaf Pair | Node HTree Weight HTree deriving (Show, Eq)
--type FreqTable = [Pair]

--- [(weight_i, char_i)], predpokladame, ze w_1 <= w_2 <= ... w_n
ex1  :: FreqTable
ex1 = [(8,"G"),(9, "R"),(11, "A"),(13,"T"),(17,"E")]

-- chceme porovnavat stromy, podla ich vahy
weight (Leaf (w,_)) = w
weight (Node left weight right) = weight 
--weight (Node left right) = weight left + weight right

instance Ord HTree where
  x < y   = weight x < weight y
  x <= y   = weight x <= weight y
  x > y   = weight x > weight y
  x >= y   = weight x >= weight y
  
build = head . (until single combine) . map Leaf
    where 
      single [x]    = True
      single _      = False

-- vahy su vzdy usporiadane, z prvych dvoch vyrobime stromcek a zaradime do zoznamu taky, aby zostal utriedeny
combine  :: [HTree] -> [HTree]
combine (t1 : t2 : ts) = insert (Node t1 ((weight t1) + (weight t2)) t2) ts

-- vseobecny insert-sort
insert      :: (Ord t) => t -> [t] -> [t]
insert u []  = [u]
insert u (t : ts) | u <= t = u : t : ts
                  | otherwise = t : insert u ts

tree1 :: HTree
tree1 = build ex1
                  
encode  :: HTree -> String -> String
encode htree xs = concat (map (\str -> case encode' htree str of
                                          Just str -> str   
                                          Nothing -> "!"
                              ) xs)
      where
          encode'  :: HTree -> Char -> Maybe String
          encode' (Leaf (_, cha)) ch   = if [ch] == cha then Just [] else Nothing
          encode' (Node left _ right) ch = case encode' left  ch  of
                                                Just le -> Just ("0" ++ le)
                                                Nothing -> case encode' right ch of
                                                                Just ri  -> Just ("1" ++ ri)
                                                                Nothing -> Nothing
                                                                --Just ("!"++ [ch] ++ "!")
                                                                

hamletLow = map toLower hamlet                                                                
hamletHtree = build (freqTable hamletLow)                                                                
hamletEncoded = encode hamletHtree hamletLow
hamletEncodedLen = (length hamletEncoded) `div` 8
hamletRawLen = length hamletLow

encodeAll :: String -> String
encodeAll xs = encode (build $ freqTable xs) xs

decode  :: HTree -> String ->  String
decode htree str = decode' htree htree str
        where
            decode'  :: HTree -> HTree -> String ->  String
            decode' orig (Leaf (_, ch)) []  = ch
            decode' orig (Leaf (_, ch)) (x:xs)  = ch ++ decode' orig orig (x:xs)
            decode' orig (Node left _ right) ('0':xs)  = decode' orig left xs
            decode' orig (Node left _ right) ('1':xs)  = decode' orig right xs
            decode' orig (Node left _ right) ('!':xs)  = error "nieco je zle"

hamletDecoded = decode hamletHtree hamletEncoded
            