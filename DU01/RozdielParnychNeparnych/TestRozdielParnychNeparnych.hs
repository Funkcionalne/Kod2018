module Main where

import qualified RozdielParnychNeparnych as F
import Test.HUnit
import System.Random

main = do g <- getStdGen
          runTestTT $ TestList [ 
              TestList [ 
                TestCase $ assertEqual "F.rozdielParnychNeparnych [1..4]" (myrozdielParnychNeparnych [1..4]) ( F.rozdielParnychNeparnych [1..4] )
                ,
                TestCase $ assertEqual "F.rozdielParnychNeparnych [1..5]" (myrozdielParnychNeparnych [1..5]) ( F.rozdielParnychNeparnych [1..5] )
              ]
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
              ,
              let lst = map (`mod` 20000) $ take 100 (randoms g :: [Integer]) in
                TestCase $ assertEqual ("F.rozdielParnychNeparnych:" ++ (show lst)) (myrozdielParnychNeparnych lst) (F.rozdielParnychNeparnych lst)
            ]
            
myrozdielParnychNeparnych	:: [Integer] -> Integer
myrozdielParnychNeparnych	xs = abs(sum ps - sum ns)
		where (ps, ns) = foldr	(\x -> \(p,n)->(n,x:p)) ([],[])	xs	
