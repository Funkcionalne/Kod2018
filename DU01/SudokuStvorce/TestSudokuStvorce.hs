module  Main where

import qualified SudokuStvorce as F
import Test.HUnit
import System.Random

s1 = [ [ 9*i+j | j<-[0..8] ] | i <- [0..8] ]
s2 = [
  [1,7,5,3,2,8,4,9,6],
  [9,4,2,6,7,1,3,8,5],
  [3,6,8,5,9,4,2,7,1],
  [8,2,9,1,3,5,6,4,7],
  [6,5,3,4,8,7,9,1,2],
  [7,1,4,9,6,2,5,3,8],
  [2,3,1,8,4,6,7,5,9],
  [4,8,7,2,5,9,1,6,3],
  [5,9,6,7,1,3,8,2,4]
  ]

main = do g <- getStdGen
          runTestTT $ TestList [ 
              TestCase $ assertEqual ("sudokuStvorce " ++ (show t)) (mysudokuStvorce t) ( F.sudokuStvorce t ) | t <- [s1, s2] 
            ]
            
mysudokuStvorce :: [[Int]] -> [[Int]]
mysudokuStvorce m    = concat [ [ [ m!!(3*i+k)!!(3*j+l) | k <- [0..2], l <- [0..2] ] | j <- [0..2] ] | i <- [0..2]]
