module Main where

import Sqrt 
import Test.HUnit
import System.Random

main = do
  g <- getStdGen
  runTestTT $ 
    TestList [ 
      TestList [ -- prva fcia
        let lst = map (abs) (randoms g :: [Float]) in
          TestList[
            TestCase $ assertEqual ("odmocnina " ++ (show x) ++ " 0.001") 
                                   True (abs ( (odmocnina (x+0.0) 0.001) 
                                               - (myodmocnina (x+0.0) 0.001)) < 0.001) 
                                   | x <- take 50 lst ]
       ]
     ]

-- riesenie tutora

myodmocnina     :: Float -> Float -> Float
myodmocnina     x eps = until (\y -> abs (y*y-x) < eps*x) improve x
                      where 
                          improve y = (y+x/y)/2
