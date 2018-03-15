module MII where

import Data.List

-- pocet nul vo vektore/matici pomocou foldr/l
pocetNul :: [Int] -> Int
pocetNul xs = foldl (\a -> \b -> if b == 0 then a + 1 else a) 0 xs

druhyNajvacsi :: [Int] -> Int        
druhyNajvacsi [] = error "prazdny zoznam"
druhyNajvacsi [x] = 0
druhyNajvacsi (x:y:xs) = snd $ foldl f (max x y, min x y) xs
                         where f (x, y) z | z < y = (x, y)
                                          | z >= x = (z, x)
                                          | otherwise = (x, z)

maximalny :: [Int] -> Int
maximalny (x:xs) = foldl (\a -> \b -> max a b) x xs


-- rozdiel max a minimalneho prvku vo vektore/matici
maxmin = undefined

maxmin' = undefined

{- --------------------------------------------------------------------
      definujte funkciu priemer :: [Float] -> Float, ktora vypocita aritmeticky zoznamu [Float]
      priemer len pouzitim foldr
-}
priemer :: [Float] -> Float
priemer xs = ssum / cc
    where (cc, ssum) = foldr f (0,0) xs
          f z (c,sum) = (c+1, sum+z)
          
priemer2 :: [Float] -> Float
priemer2 xs = uncurry (/) $ foldr f (0,0) xs
                          where f z (sum, c) = (sum+z, c+1)          

{-
foldl/r na matici
-}                  

-- priemer prvkov v matici
-- vrati sucet a pocet prvkov vo vektore

priemer'      :: [[Float]] -> Float
priemer' xss = priemer $ foldr (\row -> \y -> (priemer row) : y) [] xss

priemer''      :: [[Float]] -> Float
priemer'' xss = uncurry (/) $ foldr (\row -> \(s,c) -> let p = sucpoc row 
                                        in (s + fst p , c + snd p)) (0.0, 0.0) xss
                                         
priemer'''      :: [[Float]] -> Float
priemer''' xss = uncurry (/) $ foldr (\row -> \(s,c) -> let (rows, rowp) = sucpoc row 
                                                        in (s + rows , c + rowp)) (0.0, 0.0) xss                                         
      
sucpoc :: [Float] -> (Float, Float)
sucpoc xs = foldr f (0,0) xs
            where f z (sum, c) = (sum+z, c+1)                 
      
  
----------------------------------------------
rovnake :: [Int] -> Bool
rovnake (x:xs) = foldl (\acc -> \y -> acc && x == y) True xs

vsetkyRovnake       ::             (Eq t) =>      [t] -> Bool
vsetkyRovnake xs            = undefined

identicke             ::      (Eq t) =>      [t] -> [t] -> Bool
identicke      xs ys = undefined      

podmnozina             ::      (Eq t) =>      [t] -> [t] -> Bool
podmnozina      xs ys =       undefined

  
-------------------------------------------------

-- foldl1 f [a1,...,an] = (... (f (f a1 a2) a3 ... an)
-- foldl1 f [a1] = a1
-- foldl1 f [] = undefined
-- priklad
-- foldl1 (/) [64,4,2,8] = 1

-- definujte foldl1 pomocou foldl
foldl1' = undefined

-- foldr1 f [a1,...,an] = (f a1 (f a2 ... (f a_n-1 an)))
-- foldr1 f [a1] = a1
-- foldr1 f [] = undefined
-- priklad
-- foldr1 (/) [8,12,24,4] = 4.0

-- definujte foldr pomocou foldr1
foldr''' = undefined

-- scanl f z [a1, ..., an] = [z, f z a1, f (f z a1) a2, ...]
-- scanl (\x -> \y -> 2*x + y) 4 [1,2,3] =  [4,9,20,43] 

-- definujte scanl pomocou foldl
-- definujte scanl pomocou foldl efektivnejsie (napr. pomocou reverse)
scanl' = undefined

-- definujte foldl pomocou scanl
foldl'' = undefined

-- definujte scanr pomocou foldr
scanr' = undefined

-- definujte foldr pomocou scanr
foldr'''' = undefined

