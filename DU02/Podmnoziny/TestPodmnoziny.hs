module Main where

import qualified Podmnoziny as F
import Test.HUnit
import System.Random
import Data.List

main = do
  g <- getStdGen
  runTestTT $  
    TestList [ 
        TestCase $  assertEqual ("podmnoziny " ++ (show [1..5])) 
                               (length $ nub $  podmnoziny [1..5]) 
                               (length $ nub $  F.podmnoziny [1..5])
        ,                       
        TestCase $  assertEqual ("podmnoziny " ++ (show [1])) 
                               (length $ nub $  podmnoziny [1]) 
                               (length $ nub $  F.podmnoziny [1])
        ,                       
        TestCase $  assertEqual ("podmnoziny " ++ (show [1..10])) 
                               (length $ nub $  podmnoziny [1..10]) 
                               (length $ nub $  F.podmnoziny [1..10])
        ,                      
        TestCase $  assertEqual ("podmnozinyVPoradi " ++ (show [1])) 
                               (length $ nub $  podmnozinyVPoradi [1]) 
                               (length $ nub $  F.podmnozinyVPoradi [1])
        ,                       
        TestCase $  assertEqual ("podmnozinyVPoradi " ++ (show [1..5])) 
                               (length $ nub $  podmnozinyVPoradi [1..5]) 
                               (length $ nub $  F.podmnozinyVPoradi [1..5])
        ,                       
        TestCase $  assertEqual ("podmnozinyVPoradi " ++ (show [1..10])) 
                               (length $ nub $  podmnozinyVPoradi [1..10]) 
                               (length $ nub $  F.podmnozinyVPoradi [1..10])
       ]

-- riesenie tutora


podmnoziny :: [t] -> [[t]]
podmnoziny  []      = [[]]
podmnoziny  (x:xs)  = map (x:) subxs ++ subxs
                    where subxs = podmnoziny xs

podmnozinyVPoradi :: [t] -> [[t]]
podmnozinyVPoradi  []      = [[]]
podmnozinyVPoradi  (x:xs)  = map (x:) subxs ++ reverse subxs
                    where subxs = podmnozinyVPoradi xs

