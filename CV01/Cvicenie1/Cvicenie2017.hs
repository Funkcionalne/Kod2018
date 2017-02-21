module Cvicenie2017 where -- definicia modulu, treba pri testovaci dodrzat
import Data.Char
import Test.QuickCheck

-- typy Int, Integer, Bool, Char, String

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fib' :: Integer -> Integer
fib' n  | n < 2  = 1
        | otherwise = fib' (n-2) + fib' (n-1)
 
-- v binarnom zapise su symetricke 
binnib  :: Int -> Bool
binnib  n   = binrev n 0 == n
    where
        binrev  :: Int -> Int -> Int
        binrev  0 acc  = acc
        binrev  n acc  = binrev (n `div` 2) (2*acc+low)
                  where low = n `mod` 2
-- zoznam        
-- listcomprehension
allSym  :: Int -> [Int]
allSym n   = [ i | i<-[1..n], binnib i ]

-- type String = [Char]
int2String  :: Int -> String
int2String  0 = []      
int2String  n = int2String (n `div` 2) ++ [chr (n `mod` 2 + ord '0')]

string2Int  :: String -> Int
string2Int  s  = pom (reverse s)
            where pom []  = 0
                  pom (x:xs)  = 2*(pom xs) + (ord x - ord '0')
                  
binnib'  :: Int -> Bool
binnib' n = x == reverse x
      where x = int2String n
      
qch1 = quickCheck((\n -> (binnib n == binnib' n)))
      
qch2 = quickCheck((\n -> n>0 ==> (binnib n == binnib' n)))

-- potencna mnozina
powSet :: [t] -> [[t]]
powSet  []  = [[]]
powSet  (x:xs)  =  let ps = powSet xs in ps ++ [x:m | m <- ps] 

qch3 = quickCheck((\xs -> (length (powSet xs)) == 2^(length xs))::[Int]->Bool)
qch4 = quickCheck((\xs -> ((length xs) < 10) ==> ((length (powSet xs)) == 2^(length xs)))::[Int]->Property)

skalar  :: [Float] -> [Float] -> Float
skalar  []  []   = 0
skalar  (x:xs)  (y:ys)   = x*y + skalar xs ys

scalar  :: [Float] -> [Float] -> Float
scalar  [] [] = 0
scalar  xs ys =  sum (map2 (*) xs ys)

map2  :: (a -> b -> c) -> [a] -> [b] -> [c]
map2  f []  [] = []
map2  f (x:xs) (y:ys) = (f x y) : (map2 f xs ys)

qch5 = quickCheck((\xs -> \ys -> skalar xs ys == scalar xs ys)) 
qch6 = quickCheck((\xs -> \ys -> (length xs) == (length ys) ==> skalar xs ys == scalar xs ys)) 
qch7 = quickCheck((\xs -> \ys -> (length xs) == (length ys) ==> abs(skalar xs ys - scalar xs ys) < 0.00001)) 

--skalar [0.0,0.0,0.0,7.110755e-2,4.7109065e-6,4.2149205] [0.0,0.0,0.0,1748.2261,2.5304964e-2,0.87499666]
-- 128.00012
--scalar [0.0,0.0,0.0,7.110755e-2,4.7109065e-6,4.2149205] [0.0,0.0,0.0,1748.2261,2.5304964e-2,0.87499666]
-- 128.0001

-- I don't like floats !!!
qch8 = quickCheck((\xs -> \ys -> (length xs) == (length ys) ==> abs(skalar xs ys - scalar xs ys) < 0.001)) 

{--
skalar [0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.3186987e-5,0.0,-87.485535,9.298675,19.405136,24.313948,-168.83344]
        [0.0,0.0,0.0,0.0,0.0,0.0,0.0,18.513773,0.0,-70.686516,3.5942912,5.421243,9.312624,-9.730915]
--}

-- I hate floats !!!
qch9 = quickCheck((\xs -> \ys -> (length xs) == (length ys) ==> abs(skalar xs ys - scalar xs ys) < 1)) 
{-- :)
"?: " qch9
*** Gave up! Passed only 33 tests.
(0.00 secs, 19,212,184 bytes)
--}
