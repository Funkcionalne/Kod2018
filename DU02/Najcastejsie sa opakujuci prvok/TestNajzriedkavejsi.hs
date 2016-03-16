module Main where

import qualified Najcastejsi as F
import Test.HUnit
import System.Random
import Data.List(sort)  -- z tohoto modulu potrebujem akurat sort

main = do
  g <- getStdGen
  runTestTT $ 
    TestList [ 
      TestList [ -- prva fcia
        let lst = map (`mod` 20) (randoms g :: [Int]) in
          let x = (take 20 lst ++ ((-1):take 21 (cycle [1,2,3,4,5,6,7])) ++ take 20 lst) in
            TestList[
              TestCase $ assertEqual ("najzriedkavejsi " ++ (show x)) 
                                      (najzriedkavejsi x) 
                                      ( F.najzriedkavejsi x) ]
        ,
        let lst = map (`mod` 20) (randoms g :: [Int]) in
          let x = (-1):(take 20 lst ++ (take 21 (cycle [1,2,3,4,5,6,7])) ++ take 20 lst) in
            TestList[
              TestCase $ assertEqual ("najzriedkavejsi " ++ (show x)) 
                                      (najzriedkavejsi x) 
                                      ( F.najzriedkavejsi x) ]
       ]
     ]

-- riesenie tutora


type FreqTable t = [(Int, t)]

najcastejsi :: (Ord t) => [t] -> t
najcastejsi = snd . last . sort . chunkLengths . chunks . sort

najzriedkavejsi :: (Ord t) => [t] -> t
najzriedkavejsi = snd . head . sort . chunkLengths . chunks . sort

chunks    :: (Eq t) => [t] -> [[t]]
chunks [] = []
chunks xs@(w:_) = takeWhile (==w) xs: chunks (dropWhile (==w) xs)

chunkLengths  :: (Eq t) => [[t]] -> FreqTable t
chunkLengths xs = map (\chunk -> (length chunk, head chunk)) xs

median :: (Integral t) => [t] -> t
median xs = if n `mod` 2 == 0 then 
                (sorted !! (n `div` 2) + sorted !! (1+n `div` 2)) `div` 2
            else sorted !! (n `div` 2)
            where n = length xs
                  sorted = sort xs
                  