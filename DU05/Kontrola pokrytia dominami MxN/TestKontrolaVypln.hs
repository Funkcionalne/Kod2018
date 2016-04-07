module Main where

import qualified KontrolaVypln as F
import Test.HUnit
import System.Random
import VyplnTypy
import Data.List

main = do
  g <- getStdGen
  runTestTT $ 
    TestList [ 
      TestList [ -- prva fcia
        TestCase $ assertEqual ("kontrolaVypln 3 4 (P 1 1) (P 1 2)" ++ (show d1) )
                               ( kontrolaVypln 3 4 (P 1 1) (P 1 2) d1 )
                               ( F.kontrolaVypln 3 4 (P 1 1) (P 1 2) d1)
        ,
        TestCase $ assertEqual ("kontrolaVypln 3 4 (P 1 1) (P 1 2)" ++ (show d2))
                               ( kontrolaVypln 3 4 (P 1 1) (P 1 2) d2 )
                               ( F.kontrolaVypln 3 4 (P 1 1) (P 1 2) d2 )
        ,
        TestCase $ assertEqual ("kontrolaVypln 15 16 (P 1 1) (P 1 2)" ++ (show d3)) 
                               ( kontrolaVypln 15 16 (P 1 1) (P 1 2) d3 )
                               ( F.kontrolaVypln 15 16 (P 1 1) (P 1 2) d3 )
        ,
        TestCase $ assertEqual ("kontrolaVypln 15 16 (P 1 1) (P 1 2)" ++ (show d4))
                               ( kontrolaVypln 15 16 (P 1 1) (P 1 2) d4 )
                               ( F.kontrolaVypln 15 16 (P 1 1) (P 1 2) d4 )
        ,
        TestCase $ assertEqual ("kontrolaVypln 15 16 (P 1 1) (P 1 2)" ++ (show d5))
                               ( kontrolaVypln 15 16 (P 1 1) (P 1 2) d5 )
                               ( F.kontrolaVypln 15 16 (P 1 1) (P 1 2) d5 )
        ,
        TestCase $ assertEqual ("kontrolaVypln 50 50 (P 1 1) (P 2 1))" ++ (show d6))
                               ( kontrolaVypln 50 50 (P 1 1) (P 2 1) d6 )
                               ( F.kontrolaVypln 50 50 (P 1 1) (P 2 1) d6 )
        ,
        TestCase $ assertEqual ("kontrolaVypln 100 100 (P 1 1) (P 2 1)" ++ (show d7))
                               ( kontrolaVypln 100 100 (P 1 1) (P 2 1) d7 )
                               ( F.kontrolaVypln 100 100 (P 1 1) (P 2 1) d7 )
       ]
     ]

----

kontrolaVypln :: Int -> Int -> Pos -> Pos -> [Domino] -> Bool
kontrolaVypln m n (P x1 y1) (P x2 y2) ds =
  pocetDomin == (m*n `div` 2 - 1) && polickaOK ps && (length ps) == (m * n)
    where
      pocetDomin = length ds
      ps = (nub . sort) (dominaNaPolicka [[x1,y1], [x2, y2]] ds)
      polickaOK = foldl (\v (x:y:[])-> v && x>0 && x<=m && y>0 && y<=n) True

dominaNaPolicka :: [[Int]] -> [Domino] -> [[Int]]
dominaNaPolicka = foldl f 
  where 
    f :: [[Int]] -> Domino -> [[Int]]
    f ps (Horiz (P x y)) = [x, y] : [x, y+1] : ps 
    f ps (Vert (P x y)) = [x, y] : [x+1, y] : ps

---- generovanie riesenia

ries :: Int -> Int -> Pos -> Pos -> [Domino]
ries m n (P x1 y1) (P x2 y2) 
  | odd (m*n) || 
    even (x1 + y1 + x2 + y2) ||
    ((n == 1) && (min y1 y2 > 2) && (max y1 y2 < (m-1))) ||
    ((m == 1) && (min x1 x2 > 2) && (max x1 x2 < (n-1))) = []
  | otherwise = polozDomina (delete (P x2 y2) ((tail kon) ++ zac)) []
     where (zac,kon) = span (/= (P x1 y1)) (urobCyklus m n)

urobCyklus :: Int -> Int -> [Pos]
urobCyklus m n 
  | m == 1 = [P 1 x | x <- [1..n]]
  | n == 1 = [P x 1 | x <- [1..m]]
  | even m = [P 1 x | x <- [1..n]] ++   
             [P x n | x <- [2..m-1]] ++ 
             [P m (n-x) | x <- [0..n-1]] ++
             urobHorizHada (m-1) (n-1)
  | otherwise = [P 1 x | x <- [1..n]] ++   
                [P x n | x <- [2..m]] ++
                urobVertHada m (n-1)

urobHorizHada :: Int -> Int -> [Pos]
urobHorizHada m n = 
  concat [[P (m - r + 2) (if odd r then (n - s + 1) else s) | s <- [1..n]] | r <- [2..m]]

urobVertHada ::  Int -> Int -> [Pos]
urobVertHada m n =
  concat [[P (if odd s then (m - r + 2) else r) (n - s + 1) | r <- [2..m]] | s <- [1..n]]

polozDomina :: [Pos] -> [Domino] -> [Domino]
polozDomina [] ds = ds
polozDomina ((P x1 y1):(P x2 y2):ps) ds = polozDomina ps (d:ds)
  where d = if x1 == x2 then Horiz (P x1 (min y1 y2)) else Vert (P (min x1 x2) y1)

d1 = [Vert (P 2 1), Vert (P 2 2), Vert (P 1 4), Vert (P 1 3), Horiz (P 3 3)]
d2 = [Vert (P 1 1), Vert (P 2 2), Vert (P 1 4), Vert (P 1 3), Horiz (P 3 3)]
d3 = [Vert (P 2 1),Vert (P 4 1),Vert (P 6 1),Vert (P 8 1),Vert (P 10 1),Vert (P 12 1),
      Vert (P 14 1),Vert (P 14 2),Vert (P 12 2),Vert (P 10 2),Vert (P 8 2),Vert (P 6 2),
      Vert (P 4 2),Vert (P 2 2),Vert (P 2 3),Vert (P 4 3),Vert (P 6 3),Vert (P 8 3),
      Vert (P 10 3),Vert (P 12 3),Vert (P 14 3),Vert (P 14 4),Vert (P 12 4),Vert (P 10 4),
      Vert (P 8 4),Vert (P 6 4),Vert (P 4 4),Vert (P 2 4),Vert (P 2 5),Vert (P 4 5),
      Vert (P 6 5),Vert (P 8 5),Vert (P 10 5),Vert (P 12 5),Vert (P 14 5),Vert (P 14 6),
      Vert (P 12 6),Vert (P 10 6),Vert (P 8 6),Vert (P 6 6),Vert (P 4 6),Vert (P 2 6),
      Vert (P 2 7),Vert (P 4 7),Vert (P 6 7),Vert (P 8 7),Vert (P 10 7),Vert (P 12 7),
      Vert (P 14 7),Vert (P 14 8),Vert (P 12 8),Vert (P 10 8),Vert (P 8 8),Vert (P 6 8),
      Vert (P 4 8),Vert (P 2 8),Vert (P 2 9),Vert (P 4 9),Vert (P 6 9),Vert (P 8 9),
      Vert (P 10 9),Vert (P 12 9),Vert (P 14 9),Vert (P 14 10),Vert (P 12 10),Vert (P 10 10),
      Vert (P 8 10),Vert (P 6 10),Vert (P 4 10),Vert (P 2 10),Vert (P 2 11),Vert (P 4 11),
      Vert (P 6 11),Vert (P 8 11),Vert (P 10 11),Vert (P 12 11),Vert (P 14 11),Vert (P 14 12),
      Vert (P 12 12),Vert (P 10 12),Vert (P 8 12),Vert (P 6 12),Vert (P 4 12),Vert (P 2 12),
      Vert (P 2 13),Vert (P 4 13),Vert (P 6 13),Vert (P 8 13),Vert (P 10 13),Vert (P 12 13),
      Vert (P 14 13),Vert (P 14 14),Vert (P 12 14),Vert (P 10 14),Vert (P 8 14),Vert (P 6 14),
      Vert (P 4 14),Vert (P 2 14),Vert (P 2 15),Vert (P 4 15),Vert (P 6 15),Vert (P 8 15),
      Vert (P 10 15),Vert (P 12 15),Vert (P 14 15),Vert (P 14 16),Vert (P 12 16),Vert (P 10 16),
      Vert (P 8 16),Vert (P 6 16),Vert (P 4 16),Vert (P 2 16),Horiz (P 1 15),Horiz (P 1 13),
      Horiz (P 1 11),Horiz (P 1 9),Horiz (P 1 7),Horiz (P 1 5),Horiz (P 1 3)]
d4 = (Horiz (P 1 1)):tail d3
d5 = (Vert (P 1 1)):tail d3
d6 = ries 50 50 (P 1 1) (P 2 1)
d7 = ries 100 100 (P 1 1) (P 2 1)
