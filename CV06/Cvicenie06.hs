module Cvicenie06 where

-- http://dai.fmph.uniba.sk/courses/PARA/Prednasky/funkcionalne4.pdf

-- toto su urcite nie dobre programy
divbyzero  = 5/0

loop  = loop

infinite = 1:infinite

---

foo 0  _  = 0
foo n  x  = x/2

---

naturals = [1..]
naturals' = 1 : [ x+1 | x <-naturals']
naturals'' = naturalsFrom 1
            where naturalsFrom n = n : naturalsFrom (n+1)

evens   = [0,2..]
evens' = 0 : [ x+2 | x <-evens']
evens''  =  naturalsFromDelta 1     
            where 
            delta = 2
            naturalsFromDelta n = n : naturalsFromDelta (n+delta)
evens''' = filter (even) naturals
evens'''' = map (*2) naturals

odds  =   [1,3..]          
odds'  =   map (+1) evens          
            
{-
take 100 naturals
take 100 naturals'
-}

powers2   =  1 : [ x+x | x <-powers2 ]
-- ?? alebo
powers2'   =  1 : [ x*x | x <-powers2 ]

powers2''   =  1 : map (2^) naturals

powers2'''   =  filter (isPower2) naturals
            where 
              isPower2 1  = True
              isPower2 0  = False
              isPower2 n  = even n && isPower2 (n `div` 2)
              
powers2'''' = powers2From 1
            where  powers2From n = n : powers2From (n+n)

-------

powers n  =  1 : map (n^) naturals

-- ....

-- integers   =  0 : [ x+1, x-1 | x <- integers ]
integers   =  0 : integersFrom 1
      where integersFrom n = n : (-n) : integersFrom (n+1)
      
integers'  = 0:oneByOne naturals (map negate naturals)
      where oneByOne (x:xs) (y:ys) = x : y : oneByOne xs ys
      
---
rationals = [ (fromInteger  i) / (fromInteger  j) | i <- integers, j <- naturals]
-- preco je to zle ??

rationals' = [ (fromInteger  (s-j)) / (fromInteger  j) | s <- naturals, j <- [1..s]]

{-
gcd 0 0 = undefined
gcd a b = gcd' (abs a) (abs b)
    where gcd' a 0 = a
          gcd' a b = gcd' b (a `rem` b)
          -}

rationals'' = [ (fromInteger  (s-j)) / (fromInteger  j) | s <- naturals, j <- [1..s], gcd (s-j) j == 1]

pairs = [ ((s-j),j) | s <- naturals, j <- [1..s], gcd (s-j) j == 1]
pairs' = [ ((s-j),j) | s <- naturals, j <- [1..s]]

{-
elem (1113,1292) pairs
-}
          
-- :-)          
reals = undefined           
          
----------------------------------------------------------------

merge2  (x:xs) (y:ys) | x < y = x: merge2 xs (y:ys)
                      | y < x = y: merge2 (x:xs) ys
                      | otherwise = x: merge2 xs ys
                                            
-- "?: " take 100 $ merge2 naturals naturals
-- "?: " take 100 $ merge2 odds evens
                      
-- zovseobecnime to na lub. pocet zoznamov
merge xss =  min :  (merge ( map (dropWhile (== min)) xss))
                     where min = minimum $ map head xss
 
-- take 100 $ merge [evens, odds, powers2]
                     

cp  :: [[t]]  -> [[t]]
cp  []  = [[]]
cp  (xs: xss) = [  x:y | x <- xs,  y<-cp xss]
-- cp [ [1,2,3], [10,11], [-1,-2,-3]]

-- problem: cp [naturals,naturals]

-- riesenie aspon pre dve nekonecne mnozniny
cp2  :: [[t]]  -> [[t]]
cp2 [ xs, ys ]  = [ [xs!!(fromIntegral i), ys!!(fromIntegral j)]  | (i,j)<-pairs']
-- take 100 $ cp2 [naturals, naturals]

-- rozdel cislo na scitance
partitions :: Int -> [[Int]]
partitions  0 = [[]]
partitions  n = concat [ map (x:) (partitions (n-x)) | x<- [1..n] ]

{-
"?: " length $ partitions 10
512
-}

-- rozdel cislo na k scitancov
partitions' :: Int -> Int -> [[Int]]
partitions'  0 0 = [[]]
partitions'  0 _ = [] -- neda sa k > 0
partitions'  n 0 = []   -- neda sa
partitions'  n k = concat [ map (x:) (partitions' (n-x) (k-1)) | x<- [1..n] ]

cp'  :: [[t]]  -> [[t]]
cp' xss  = [   [  xs!!i | (xs, i) <- zip xss p ]   |
                    s <- [1..],
                    p <- partitions' s (length xss)
           ]

-- take 100 $ cp' [evens, odds, evens]

powers23  =   1 : merge2 [ 2*x | x<-powers23] [ 3*x | x<-powers23]
