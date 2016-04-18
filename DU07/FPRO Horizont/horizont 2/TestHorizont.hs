module Main where

import qualified Horizont as F
import Test.HUnit

main = do
   runTestTT $ 
      TestList [ 
        TestList [ -- prva fcia
          TestCase $ assertEqual ("horizont " ++ (show h10)) (horizont h10) ( F.horizont h10 )
          ,
          TestCase $ assertEqual ("horizont " ++ (show h11)) (horizont h11) ( F.horizont h11 )         
          ,
          TestCase $ assertEqual ("horizont " ++ (show h13)) (horizont h10) ( F.horizont h13 )         
        ]
      ]


------------ riesenie

horizont :: [Int] -> [Int]
horizont = invuprav . horizont' . uprav 

{- 
aby sme nemali v dalsom starosti s relativnymi rozmermi obdlznikov,
prerobime na absolutne 
-}
uprav :: [Int] -> [(Int,Int)]
uprav [] = []
uprav (x:y:d:xs) = (x, y) : (x+d, 0) : uprav xs

invuprav :: [(Int,Int)] -> [Int]
invuprav = (0:) . invuprav'

invuprav' :: [(Int,Int)] -> [Int]
invuprav' [] = []
invuprav' [(x,y)] = [x,y]
invuprav' ((x1,y1):(x2,y2):zs) = 
  if x1 == x2 then invuprav' ((x1, (max y1 y2)) : zs)
              else x1 : y1 : invuprav' ((x2, y2) : zs)

-- horizonty spracujeme sposobom ako pri triedeni zlucovanim
horizont' :: [(Int,Int)] -> [(Int,Int)]
horizont' [] = []
horizont' [x,y] =[x,y]
horizont' h = spojHoriz 0 0 (horizont' (take dh h)) 0 (horizont' (drop dh h))
  where
    dh = 2 *((length h) `div` 4)

-- predchadzajuca vyska, posledna vyska v prvom horiz., 1. horiz, 
--                       posledna vyska v duhom horiz., 2. horiz.    
spojHoriz :: Int -> Int -> [(Int,Int)] -> Int -> [(Int,Int)] -> [(Int,Int)]
spojHoriz _ _ h _ [] = h
spojHoriz _ _ [] _ h = h
spojHoriz v v1 h1@((x1,y1):hs1) v2 h2@((x2,y2):hs2)
  | x1 <= x2   = let novev = max y1 v2 in 
                  if v /= novev then (x1, novev) : spojHoriz novev y1 hs1 v2 h2
                                else spojHoriz v y1 hs1 v2 h2
  | otherwise = let novev = max v1 y2 in
                  if v /= novev then (x2, novev) : spojHoriz novev v1 h1 y2 hs2
                                else spojHoriz v v1 h1 y2 hs2
   
o1 = [2,2,3] :: [Int]
o2 = [3,1,4] :: [Int]
o3 = [1,3,2] :: [Int]
o4 = [3,4,1] :: [Int]
o5 = [3,1,1] :: [Int]
o6 = [6,1,1] :: [Int]

h1 = o1 ++ o2
h2 = o1 ++ o3
h3 = o1 ++ o4
h4 = o1 ++ o5
-- (1,11,5), (2,6,7), (3,13,9), (12,7,16), (14,3,25),(19,18,22), (23,13,29), (24,4,28))
h5 = [1,11,4, 2,6,5, 3,13,6, 12,7,4, 14,3,11, 19,18,3, 23,13,6, 24,4,4] :: [Int]
h6 = o1 ++ o6
h7 = [1,11,4, 2,6,5, 3,13,6, 12,7,4] :: [Int]
h8 = [1,11,4, 2,6,5] :: [Int]
h9 = [3,13,6, 12,7,4] :: [Int]

genHoriz :: Int -> [Int]
genHoriz 0 = []
genHoriz n = n : n : n : genHoriz (n-1)

h10 = genHoriz 100000
h11 = reverse h10
h12 = (drop 333 h10) ++ (take 333 h10)
h13 = (drop 333 h12) ++ (take 333 h12)