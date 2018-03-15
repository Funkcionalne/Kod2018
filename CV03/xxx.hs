{--
data  Step  = Left | Right deriving(Show, Eq)
type Path = [Step]

trace (Tip x) []  = x
trace (Bin left right) (Left : ps)   = trace left ps
trace (Bin left right) (Right : ps)   = trace right ps
--}

type Weight = Int
type Pair = (Char, Weight)
data HTree = Leaf Pair | Node HTree Weight HTree deriving (Show, Eq)
--- coding [(c_i, w_i)], predpokladame, ze w_1 <= w_2 <= ... w_n
ex1  :: [Pair]
ex1 = [('G',8),('R',9),('A',11),('T',13),('E',17)]
-- chceme porovnavat stromy, podla ich vahy
weight (Leaf (_,w)) = w
weight (Node left weight right) = weight 
--weight (Node left right) = weight left + weight right

instance Ord HTree where
  x < y   = weight x < weight y
  x <= y   = weight x <= weight y
  x > y   = weight x > weight y
  x >= y   = weight x >= weight y
  

build = head . (until single combine) . map Leaf
    where 
      single [x]   = True
      single _   = False

-- vahy su vzdy usporiadane
combine  :: [HTree] -> [HTree]
combine (t1 : t2 : ts) = insert (Node t1 ((weight t1) + (weight t2)) t2) ts

insert  :: (Ord t) => t -> [t] -> [t]
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

encode'  :: HTree -> Char -> Maybe String
encode' (Leaf (cha, _)) ch   = if ch == cha then Just [] else Nothing
encode' (Node left _ right) ch = case encode' left  ch  of
                                      Just le -> Just ("0" ++ le)
                                      Nothing -> case encode' right ch of
                                                      Just ri  -> Just ("1" ++ ri)
                                                      Nothing -> Nothing
                                      
--decode  :: String -> HTree -> String