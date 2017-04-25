module Max where

nahradMax :: (Ord t) => [t] -> [t]
nahradMax xs = replicate (length xs) (maximum xs)
{-
"?: " nahradMax [1..10]
[10,10,10,10,10,10,10,10,10,10]
-}               

nahradMax' :: (Ord t) => [t] -> [t]
nahradMax' (x:xs) = fst $ jedenPrechod xs x
          where jedenPrechod [] m     = ([] , m)
                jedenPrechod (x:xs) m = (m':ys, m')
                    where (ys, m') = jedenPrechod xs (max x m)

nahradAvg :: [Float] -> [Float]
nahradAvg xs = fst $ jedenPrechod xs 0
          where jedenPrechod [] s     = ([] , s)
                jedenPrechod (x:xs) s = ((x/s'):ys, s')
                    where (ys, s') = jedenPrechod xs (x+s)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Read)

nahradMaxTree :: (Ord t) => Tree t -> Tree t
nahradMaxTree t = nahrad (maxTree t) t
  where nahrad _ Empty = Empty
        nahrad m (Branch _ t1 t2) = (Branch m (nahrad m t1) (nahrad m t2))
        --
        maxTree :: (Ord a) => Tree a -> a
        maxTree (Branch x Empty Empty) = x 
        maxTree (Branch x Empty t2) = max x (maxTree t2)
        maxTree (Branch x t1 Empty) = max x (maxTree t1)
        maxTree (Branch x t1 t2) = max x (max (maxTree t1) (maxTree t2))
                
nahradMaxTree' :: (Ord t) => Tree t -> Tree t
nahradMaxTree'  (Branch k t1 t2) = fst $ jedenPrechod  k t1
  where jedenPrechod m Empty = (Empty, m)
        jedenPrechod m (Branch k t1 t2) = ((Branch m'' t1' t2'), m'')
          where
            (t1', m') = jedenPrechod (max k m) t1
            (t2', m'') = jedenPrechod m' t2

t1 = Empty
t2 = Branch 1 t1 t1
t3 = Branch 2 t2 t2
t4 = Branch 3 t3 t3
t5 = Branch 4 t4 t4
t6 = Branch 5 t5 t5
t7 = Branch 6 t6 t6
t8 = Branch 7 t7 t7
t9 = Branch 8 t8 t8
t10 = Branch 9 t9 t9
t11 = Branch 10 t10 t10
t12 = Branch 11 t11 t11
t13 = Branch 12 t12 t12
t14 = Branch 13 t13 t13
t15 = Branch 14 t14 t14
t16 = Branch 15 t15 t15
t17 = Branch 16 t16 t16
t18 = Branch 17 t17 t17
t19 = Branch 18 t18 t18
t20 = Branch 19 t19 t19            

{-

sucet :: (Num a) => Tree a -> a
sucet Empty = 0
sucet (Branch x t1 t2) = x + sucet t1 + sucet t2

percenta :: (Fractional a) => Tree Integer -> Tree a
percenta t = percenta' (sucet t) t
  where
    percenta' :: (Fractional a) => Integer -> Tree Integer -> Tree a
    percenta' _ Empty = Empty
    percenta1 s (Branch x t1 t2) = 
        Branch (fromInteger x / fromInteger s) (percenta' s t1) (percenta' s t2)
                    
                    

  -}                  
                    
percenta' :: (Fractional a) => Tree Integer -> Tree a
percenta' t = t'
  where (s, t') = percenta1' s t
  
percenta1' :: (Fractional a) => Integer -> Tree Integer -> (Integer, Tree a)
percenta1' s Empty = (0, Empty)
percenta1' s (Branch v t1 t2) = 
    (v + s1 + s2, Branch (fromInteger v / fromInteger s) t1' t2')
  where
    (s1, t1') = percenta1' s t1
    (s2, t2') = percenta1' s t2                    