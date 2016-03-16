module Main where

import qualified Postupnosti as F
import Test.HUnit
import System.Random

main = do
  g <- getStdGen
  runTestTT $  
      TestList [ 
        let lst = take 10 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("nsrp " ++ (show lst)) 
                                   (nsrp lst) 
                                   (F.nsrp lst)
        ,    
        let lst = take 20 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("nsrp " ++ (show lst)) 
                                   (nsrp lst) 
                                   (F.nsrp lst)
        ,    
        let lst = take 30 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("nsrp " ++ (show lst)) 
                                   (nsrp lst) 
                                   (F.nsrp lst)
        ,    
        let lst = take 100 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("nsrp " ++ (show lst)) 
                                   (nsrp lst) 
                                   (F.nsrp lst)
        ,    
        let lst = take 500 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("nsrp " ++ (show lst)) 
                                   (nsrp lst) 
                                   (F.nsrp lst)
        ,    
        let lst = take 1000 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("nsrp " ++ (show lst)) 
                                   (nsrp lst) 
                                   (F.nsrp lst)
        ,    
        let lst = take 10000 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("nsrp " ++ (show lst)) 
                                   (nsrp lst) 
                                   (F.nsrp lst)
        ,    
        let lst = take 100000 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("nsrp " ++ (show lst)) 
                                   (nsrp lst) 
                                   (F.nsrp lst)
        ,    
        let lst = take 1000000 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("nsrp " ++ (show lst)) 
                                   (nsrp lst) 
                                   (F.nsrp lst)
       ]
        
-- riesenie tutora

nsrp   :: (Ord t) => [t] -> Int
nsrp  []  = -1
nsrp  (x:xs) = maxLen where
        (maxLen, _, _) = foldl pom (1,1,x)  xs 
        pom (maxLen, len, previous) x = if x > previous then 
                                            (max maxLen (len+1),len+1,x)
                                        else 
                                            (max maxLen 1,1,x)
 