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

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

nahradMaxTree :: (Ord t) => Tree t -> Tree t
nahradMaxTree t = nahrad (maxTree t) t
  where nahrad _ Nil = Nil
        nahrad m (Node _ t1 t2) = (Node m (nahrad m t1) (nahrad m t2))
        --
        maxTree :: (Ord a) => Tree a -> a
        maxTree (Node x Nil Nil) = x 
        maxTree (Node x Nil t2) = max x (maxTree t2)
        maxTree (Node x t1 Nil) = max x (maxTree t1)
        maxTree (Node x t1 t2) = max x (max (maxTree t1) (maxTree t2))
                
nahradMaxTree' :: (Ord t) => Tree t -> Tree t
nahradMaxTree'  t@(Node k t1 t2) = fst $ jedenPrechod  k t
  where jedenPrechod m Nil = (Nil, m)
        jedenPrechod m (Node k t1 t2) = ((Node m'' t1' t2'), m'')
          where
            (t1', m') = jedenPrechod (max k m) t1
            (t2', m'') = jedenPrechod m' t2

t1 = Nil
t2 = Node 1 t1 t1
t3 = Node 2 t2 t2
t4 = Node 3 t3 t3
t5 = Node 4 t4 t4
t6 = Node 5 t5 t5
t7 = Node 6 t6 t6
t8 = Node 7 t7 t7
t9 = Node 8 t8 t8
t10 = Node 9 t9 t9
t11 = Node 10 t10 t10
t12 = Node 11 t11 t11
t13 = Node 12 t12 t12
t14 = Node 13 t13 t13
t15 = Node 14 t14 t14
t16 = Node 15 t15 t15
t17 = Node 16 t16 t16
t18 = Node 17 t17 t17
t19 = Node 18 t18 t18
t20 = Node 19 t19 t19            

{-

sucet :: (Num a) => Tree a -> a
sucet Nil = 0
sucet (Node x t1 t2) = x + sucet t1 + sucet t2

percenta :: (Fractional a) => Tree Integer -> Tree a
percenta t = percenta' (sucet t) t
  where
    percenta' :: (Fractional a) => Integer -> Tree Integer -> Tree a
    percenta' _ Nil = Nil
    percenta1 s (Node x t1 t2) = 
        Node (fromInteger x / fromInteger s) (percenta' s t1) (percenta' s t2)
                    
                    

  -}                  
                    
percenta' :: (Fractional a) => Tree Integer -> Tree a
percenta' t = t'
  where (s, t') = percenta1' s t
  
percenta1' :: (Fractional a) => Integer -> Tree Integer -> (Integer, Tree a)
percenta1' s Nil = (0, Nil)
percenta1' s (Node v t1 t2) = 
    (v + s1 + s2, Node (fromInteger v / fromInteger s) t1' t2')
  where
    (s1, t1') = percenta1' s t1
    (s2, t2') = percenta1' s t2                    