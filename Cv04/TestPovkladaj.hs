module Main where

import qualified Povkladaj as F
import Test.HUnit
import System.Random
import Data.List

main = do
  g <- getStdGen
  runTestTT $  
      TestList (
      [  
            TestCase $ assertEqual ("povkladaj 0 " ++ show lab ) (length $ mypovkladaj 0 lab) (length $ F.povkladaj 0 lab)
            | lab <- [
            [],
            [1],
            [1,2],
            [1,2,3,4],
            [0,1,2],
            [1..10]
            ]::[[Int]]            
       ]
       ++
       [  
            TestCase $ assertEqual ("povkladaj 0 " ++ show lab ) (True) (all (elem 0) $ F.povkladaj 0 lab)
            | lab <- [
            [],
            [1],
            [1,2],
            [1,2,3,4],
            [0,1,2],
            [1..10]
            ]::[[Int]]            
       ]
       ++
       [  
            TestCase $ assertEqual ("povkladaj 0 " ++ show lab ) (True) (all (\x -> length x == 1+length lab) $ F.povkladaj 0 lab)
            | lab <- [
            [],
            [1],
            [1,2],
            [1,2,3,4],
            [0,1,2],
            [1..10]
            ]::[[Int]]            
       ]
       ++
       [  
            TestCase $ assertEqual ("povkladaj 0 " ++ show lab ) (F.povkladaj 0 lab) (nub $ F.povkladaj 0 lab)
            | lab <- [
            [],
            [1],
            [1,2],
            [1,2,3,4],
            [(-1),1,2],
            [1..10]
            ]::[[Int]]            
       ]
       )

        
-- riesenie tutora 
mypovkladaj 	     	:: t -> [t] -> [[t]]
mypovkladaj x []  	= [[x]]
mypovkladaj x (y:ys) = (x:y:ys) : map (y:) (mypovkladaj x ys)

