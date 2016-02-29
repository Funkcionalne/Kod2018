{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main where

import qualified Cifry as F
import Test.HUnit
import System.Random

main = do g <- getStdGen
          runTestTT $ TestList [ 
              TestList [ 
                TestCase $ assertEqual "F.cifry 1234" (mycifry 1234) ( F.cifry 1234 )
                ,
                TestCase $ assertEqual "F.cifryR 1234" (mycifryR 1234) ( F.cifryR 1234 )
              ],
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestList[TestCase $ assertEqual ("F.cifry:" ++ (show x)) (mycifry x) (F.cifry x) | x <- lst],
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestList[TestCase $ assertEqual ("F.cifry:" ++ (show x)) (mycifryR x) (F.cifryR x) | x <- lst]
            ]
            
mycifry	:: Integer -> [Integer]			
mycifry 0 = []
mycifry n = mycifry (n `div` 10) ++ [n `mod` 10]

mycifryR	:: Integer -> [Integer]			
mycifryR 0 = []
mycifryR n = (n `mod` 10):mycifryR (n `div` 10)
