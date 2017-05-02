module Percenta where
-- pustite si vsetky verzie postupne pre rovnaky zoznam
-- napr maximum (nahrad [1..10000]), atd az po nahrad'''
-- vsimnite si cas a pouzitu pamat

-- najprv divno
nahrad xs = xs'
  where (xs', m) = nahradMax xs 0
  
nahradMax [] m = ([], m)
nahradMax (x:xs) m = (m' : xs', m')
  where
    (xs', m') = nahradMax xs (if x > m then x else m)
    
    

    
-- zvysne su uz len rozne sposoby preliezania zoznamom
nahrad' xs = prepis xs 
  where
    m = maximum xs
    prepis [] = []
    prepis (x':xs')= m : prepis xs'
    
mojmax [] = 0
mojmax (x:xs) = if x > m then x else m
  where 
    m = mojmax xs

nahrad'' xs = prepis xs 
  where
    m = mojmax xs
    prepis [] = []
    prepis (_:xs') = m : prepis xs'    

mojmax' xs = mm xs 0
  where
    mm [] m = m
    mm (x:xs) m = mm xs (if x > m then x else m)

nahrad''' xs = prepis xs 
  where
    m = mojmax' xs
    prepis [] = []
    prepis (_:xs') = m : prepis xs'    
    

data  Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Read)    
percenta' :: (Fractional a) => Tree Integer -> Tree a
percenta' t = t'
  where (s, t') = percenta1' s t
  
percenta1' :: (Fractional a) => Integer -> Tree Integer -> (Integer, Tree a)
percenta1' _ Empty = (0, Empty)
percenta1' s (Branch v t1 t2) = 
    (v + s1 + s2, Branch (fromInteger v / fromInteger s) t1' t2')
  where
    (s1, t1') = percenta1' s t1
    (s2, t2') = percenta1' s t2    
    
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

sucet :: (Num a) => Tree a -> a
sucet Empty = 0
sucet (Branch x t1 t2) = x + sucet t1 + sucet t2
