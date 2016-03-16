module Main where

import qualified DropIndex as F
import Test.HUnit
import System.Random

main = do
  g <- getStdGen
  runTestTT $  
      TestList [ 
        let lst = take 10 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("drop 5 " ++ (show lst)) 
                                   (drop 5 lst) 
                                   (F.drop' 5 lst)
        ,    
        let lst = take 10 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ((show lst) ++ " !!!! 5 " ) 
                                   (lst !! 5) 
                                   (lst F.!!!! 5 )
        ,
        let lst = take 100 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("drop 5 " ++ (show lst)) 
                                   (drop 5 lst) 
                                   (F.drop' 5 lst)
        ,    
        let lst = take 100 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ((show lst) ++ " !!!! 5 " ) 
                                   (lst !! 5) 
                                   (lst F.!!!! 5 )
        ,
        let lst = take 10 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("drop 0 " ++ (show lst)) 
                                   (drop 0 lst) 
                                   (F.drop' 0 lst)
        ,    
        let lst = take 10 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ((show lst) ++ " !!!! 0 " ) 
                                   (lst !! 0) 
                                   (lst F.!!!! 0 )
        ,
        let lst = take 10 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("drop 10 " ++ (show lst)) 
                                   (drop 10 lst) 
                                   (F.drop' 10 lst)
        ,    
        let lst = take 10 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ((show lst) ++ " !!!! 9 " ) 
                                   (lst !! 9) 
                                   (lst F.!!!! 9 )
        ,
        let lst = take 10000 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ("drop 5555 " ++ (show lst)) 
                                   (drop 5555 lst) 
                                   (F.drop' 5555 lst)
        ,    
        let lst = take 10000 $ map (`mod` 20) $ map (abs) (randoms g :: [Int]) in
            TestCase $ assertEqual ((show lst) ++ " !!!! 5555 " ) 
                                   (lst !! 5555) 
                                   (lst F.!!!! 5555 )
      ]
        
-- riesenie tutora

drop'       :: Int -> [t] -> [t]
drop' n xs  =  (foldr pom (\_ -> []) xs) 0 where  
                pom x h = \m -> if m < n then h (m+1)
                                 else x:(h (m+1))

(!!!!)      :: [t] -> Int -> t
xs !!!! n   =  (foldr pom (\_ -> error "nie je") xs) 0 where  
                pom x h = \m -> if m == n then x
                                else h (m+1)

take' :: Int -> [a] -> [a]
take' n xs  =  (foldr pom (\_ -> []) xs) n where  
                pom x h = \n -> if n == 0 then []
                                 else x:(h (n-1))
