module Main where

import qualified Cislo as F
import Test.HUnit
import System.Random

main = do
  g <- getStdGen
  runTestTT $ 
    TestList [ 
      TestList [ -- prva fcia
        TestCase $ assertEqual "cisla 2016 2106" 
                               0
                               ( F.cislo 2016 2016 )
        ,
        TestCase $ assertEqual "cisla 2016 2017" 
                               2174 
                               ( F.cislo 2016 2017) 
        ,
        TestCase $ assertEqual "cisla 100! 2017" 
                               116594297758780824377895721384398037924560565175871452812684090314604460005519564510269098896859392207192517038789682698224243764932687372254187011756907298816
                               ( F.cislo (fak 100) 2017 )
        ,
        let lst = map (`mod` 100000) $ take 20 (randoms g :: [Integer]) in
          TestList[
            TestCase $ assertEqual ("cisla " ++ (show x) ++ " " ++ (show x)) 
                                   (cislo x x) 
                                   (F.cislo x x) | x <- lst]
       ]
     ]

-- riesenie tutora

g :: Integer -> Integer -> Integer
g 0 m = m
g n m 
  | odd n  = g (n `div` 2) (2 * m + 1)
  | even n = g (n `div` 2) (2 * m)
  
f :: Integer -> Integer
f n = g n 0

cislo :: Integer -> Integer -> Integer
cislo n x = if even x then 0 else uprav (g x 0)
  where 
    uprav x = if x > n then x else uprav ( 2*x )
 
fak :: Integer -> Integer
fak n = if n < 2 then 1 else n * fak (n-1)