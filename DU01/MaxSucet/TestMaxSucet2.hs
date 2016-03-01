module Main where

import qualified MaxSucet as F
import Test.HUnit
import System.Random

main = do
  g <- getStdGen
  runTestTT $ 
    TestList [ 
      TestList [ -- prva fcia
        TestCase $ assertEqual "maxSucet []" 
                               (0,[]) 
                               ( F.maxSucet [] )
        ,
        TestCase $ assertEqual "maxSucet [-2, 1, -3, 4, -1, 2, 1, -5, 4]" 
                               (6,[4,-1,2,1]) 
                               ( F.maxSucet [-2, 1, -3, 4, -1, 2, 1, -5, 4] )
        ,
        let lst = map (`mod` 2000) (randoms g :: [Int]) in
          TestList[
            TestCase $ assertEqual ("maxSucet " ++ (show x)) 
                                   (maxSucet x) 
                                   (F.maxSucet x) | x <- take 20 (urobZoznamy lst)]
       ]
     ]

-- riesenie tutora

urobZoznamy :: [Int] -> [[Int]]
urobZoznamy (x:xs) = uz x xs []

uz :: Int -> [Int] -> [Int] -> [[Int]]
uz n (x:xs) vys 
  | n == 0 = vys:(uz x xs [])
  | otherwise = uz (n-1) xs ((if x `mod` 3 == 0 then x else negate x):vys)
  
maxSucet :: [Int] -> (Int, [Int])
maxSucet = ms 0 [] 0 [] 

ms :: Int -> [Int] -> Int -> [Int] -> [Int] -> (Int, [Int])
ms maxsem ys maxcelkovo zs []     = (maxcelkovo, zs)
ms maxsem ys maxcelkovo zs (x:xs) = ms maxsem' ys' maxcelkovo' zs' xs
   where
     (maxsem', ys')     = if maxsem + x > 0 then (maxsem + x, ys ++ [x]) else (0, [])
     (maxcelkovo', zs') = if maxcelkovo > maxsem' then (maxcelkovo, zs) else (maxsem', ys')  

msn :: [Int] -> (Int, [Int])
msn  = vyberMaximum . zistiMaxima . zaciatkyZoznamu

zaciatkyZoznamu :: [Int] -> [[Int]]
zaciatkyZoznamu []       = [[]]
zaciatkyZoznamu z@(x:xs) = [z] ++ zaciatkyZoznamu xs

zistiMaxima :: [[Int]] -> [(Int, [Int])]
zistiMaxima [[]] =[]
zistiMaxima (x:xs) = zistiMaximum x ++ zistiMaxima xs

zistiMaximum :: [Int] -> [(Int, [Int])]
zistiMaximum = zm 0 []

zm :: Int -> [Int] -> [Int] -> [(Int, [Int])]
zm s z []     = [] 
zm s z (x:xs) = (s', z'):(zm s' z' xs)
  where
    s' = s + x
    z' = z ++ [x]

vyberMaximum :: [(Int, [Int])] -> (Int, [Int])
vyberMaximum []     = (0,[])
vyberMaximum ((m,z):xs) = if m > m' then (m, z) else (m', z')
  where
    (m', z') = vyberMaximum xs