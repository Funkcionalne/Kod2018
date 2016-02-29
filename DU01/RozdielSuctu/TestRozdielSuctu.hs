module Main where

import qualified RozdielSuctu as F
import Test.HUnit
import System.Random

main = do
  g <- getStdGen
  runTestTT $ 
    TestList [ 
      TestList [ -- prva fcia
        TestCase $ assertEqual "rozdielSuctu []" 
                               0
                               ( F.rozdielSuctu [] )
        ,
        TestCase $ assertEqual "rozdielSuctu [1]" 
                               (0-1)
                               ( F.rozdielSuctu [1] )
        ,
        TestCase $ assertEqual "rozdielSuctu [1,2]" 
                               1 
                               ( F.rozdielSuctu [1,2] )
        ,
        TestCase $ assertEqual "rozdielSuctu [1,2,3]" 
                               (0-2)
                               ( F.rozdielSuctu [1,2,3] )
        ,
        TestCase $ assertEqual "rozdielSuctu [2,0,1,6]" 
                               3 
                               ( F.rozdielSuctu [2,0,1,6] )
        ,
        TestCase $ assertEqual "rozdielSuctu [1..10000]" 
                               50000 
                               ( F.rozdielSuctu [1..100000] )
        ,
        let lst = map (`mod` 200) $ take 2000 (randoms g :: [Integer]) in
          TestList[
            TestCase $ assertEqual ("rozdielSuctu " ++ (show lst)) 
                                   (rozdielSuctu lst) 
                                   (F.rozdielSuctu lst)] 
       ]
     ]

-- riesenie tutora

rozdelParneNeparne :: [Integer] -> ([Integer],[Integer])
rozdelParneNeparne [] = ([],[])
rozdelParneNeparne (x:xs) = (xp, x:xn)
  where 
    (xp, xn) = rozdelNeparneParne xs

rozdelNeparneParne :: [Integer] -> ([Integer],[Integer])
rozdelNeparneParne [] = ([],[])
rozdelNeparneParne (x:xs) = (x:xp, xn)
  where 
    (xp, xn) = rozdelParneNeparne xs

rozdielSuctu :: [Integer] -> Integer
rozdielSuctu xs = sum parneMiesta - sum neparneMiesta
  where
    (parneMiesta, neparneMiesta) = rozdelParneNeparne xs
