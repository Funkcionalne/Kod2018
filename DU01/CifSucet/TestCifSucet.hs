module Main where

import qualified CifSucet as F
import Test.HUnit
import System.Random

main = do
  g <- getStdGen
  runTestTT $ 
    TestList [ 
      TestList [ -- prva fcia
        TestCase $ assertEqual "jCislaPocet 8 18" 
                               1
                               ( F.jCislaPocet 8 18 )
        ,
        TestCase $ assertEqual "jCislaPocet fak30 (fak30+1000)" 
                               111 
                               ( F.jCislaPocet fak30 (fak30+1000) )
        ,
        TestCase $ assertEqual "jCislaPocet 5 14" 
                               2 
                               ( F.jCislaPocet 5 14 )
        ,
        let lst = map (`mod` 2000) $ take 40 (randoms g :: [Integer]) in
          TestList[
            TestCase $ assertEqual ("jCislaPoc " ++ (show x) ++ " " ++ (show (x+y))) 
                                   (jCislaPocet x (x+y)) 
                                   (F.jCislaPocet x (x+y)) | (x, y) <- urobDvojice lst]
       ]
     ]

-- riesenie tutora

--myFact :: Integer -> Integer
myfact n = myfact' n 1

--myFact' :: Integer -> Integer -> Integer
myfact' 0 acc = acc
myfact' n acc = myfact' (n-1) (n*acc)

fak30 :: Integer
fak30 = myfact 30

jCislaPoc :: Integer -> Integer -> Integer -> Integer
jCislaPoc a b c = d `div` 9 + x
  where
     d  = (b - a + 1)
     x  = if (d `mod` 9 > 0) && (c' <= b') then 1 else 0
     a' = a `mod` 9
     b' = (b + 9 - a') `mod` 9    
     c' = (c + 9 - a') `mod` 9   -- b a c posunieme tak akoby a' bola 0

jCislaPocet :: Integer -> Integer -> Integer
jCislaPocet a b = jCislaPoc a b 5

urobDvojice :: [Integer] -> [(Integer, Integer)]
urobDvojice []       = []
urobDvojice (x:y:xs) = (x,y):urobDvojice xs