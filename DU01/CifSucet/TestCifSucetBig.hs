module Main where

import qualified CifSucet as F
import Test.HUnit
import System.Random

main = do
  g <- getStdGen
  sequence [ 
      do 
        putStrLn "testing1..."
        runTestTT $ TestCase $ assertEqual "jCislaPocet fak1000 (fak1000+1000)" 
                               1111 
                               ( F.jCislaPocet fak1000 (fak1000+10000) )
      ,
      do 
        putStrLn "testing2..."
        runTestTT $ TestCase $ assertEqual "jCislaPocet fak1000 (fak1000+10000)" 
                               1111 
                               ( F.jCislaPocet fak1000 (fak1000+100000) )

                               ]
  putStrLn "testing2..."
  let lst = map (`mod` 200) $ take 40 (randoms g :: [Integer]) in
    do 
       putStrLn "testing2..."
       --last $ map ( \x -> runTestTT (TestList x) )   [
       runTestTT $ 
        TestLabel "kokotina" $
          TestList
            [TestCase $ assertEqual ("jCislaPocet " ++ (show x) ++ " " ++ (show (x+y))) 
                                   (jCislaPocet x (x+y)) 
                                   (F.jCislaPocet x (x+y)) | (x, y) <- urobDvojice lst ]
  putStrLn "testing3..."
  runTestTT $                                
    TestList [ 
      TestList [ -- prva fcia
        TestCase $ assertEqual "jCislaPocet fak1000 (fak1000+1000)" 
                               1111 
                               ( F.jCislaPocet fak1000 (fak1000+10000) )
        ,
        let lst = map (`mod` 200) $ take 40 (randoms g :: [Integer]) in
          TestList[
            TestCase $ assertEqual ("jCislaPocet " ++ (show x) ++ " " ++ (show (x+y))) 
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

fak1000 :: Integer
fak1000 = myfact 1000

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
urobDvojice (x:y:xs) = (myfact x, 100 * y):urobDvojice xs