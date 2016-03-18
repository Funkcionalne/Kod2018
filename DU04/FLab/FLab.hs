module FLab where

type  Lab = [[Int -> Int]]

findMaxPath :: Lab -> Int -> Int
findMaxPath lab init = maximum $ map (\path -> apply lab path init) (cesta' m n)
                       where m = length lab
                             n = length $ head $ lab

findMaxPath' :: Lab -> Int -> [Int]
findMaxPath' lab init = map (\path -> apply lab path init) (cesta' m n)
                       where m = length lab
                             n = length $ head $ lab

findMaxPath'' :: Lab -> Int -> [Path]
findMaxPath'' lab init = (cesta' m n)
                       where m = length lab
                             n = length $ head $ lab
                             
type Path = [(Int,Int)]

lab1  :: Lab
lab1  = [
          [ (+1), (+1),(+1) ],
          [ (+1), (+1),(+1) ],
          [ (+1), (*2),(+1) ]
        ]
-- findMaxPath lab1 0 = 15

lab2  :: Lab
lab2  = [
          [ (+1), (+1),(+1) ],
          [ (+1), (+1),(*2) ],
          [ (+1), (*2),(+1) ]
        ]
-- findMaxPath lab2 0 = 23

lab3  :: Lab
lab3  = [
          [ (+1), (+1),(+1) ],
          [ (*2), (+1),(*2) ],
          [ (+1), (*2),(+1) ]
        ]
-- findMaxPath lab3 0 = 31

lab4  :: Lab
lab4  = [
          [ (+1), (*2),(*3) ],
          [ (*2), (`div` 3),(*2) ],
          [ (+4), (*2),(+10) ]
        ]
-- findMaxPath lab4 0 = 108
-- [(0,2),(1,2),(2,2),(2,1),(2,0),(1,0),(1,1),(0,1),(0,0)]

lab5  :: Lab
lab5  = [
          [ (*2), (*3),(*5) ],
          [ (*8), (*9),(*4) ],
          [ (*7), (*6),(*10) ]
        ]

-- findMaxPath lab5 1 = 3628800

apply :: Lab -> Path -> Int -> Int
apply lab path init = foldr (\(i,j) -> \f -> (lab!!i!!j) . f ) id path init

susedne   :: [(Int->Int,Int->Int)]
susedne   = [((+1),id), ((subtract 1),id), (id, (+1)), (id, (subtract 1))]

cesta'   :: Int -> Int -> [Path]
cesta' m n = cesta (m*n-1) m n [(0,0)]

cesta   :: Int -> Int -> Int -> Path -> [Path]
cesta  0 _ _ path = [path]
cesta  len m n path = concat $ map (\(ii,jj) -> cesta (len-1) m n ((ii,jj):path))
                          [(f1 i, f2 j) | (f1, f2) <- susedne, notElem ((f1 i),(f2 j)) path, f1 i>=0, f1 i < m, f2 j>=0, f2 j <n]
                  where (i,j) = head path

{-
"?: " length $ c 5 5
824
(1.37 secs, 476,287,088 bytes)
"?: " length $ c 6 6
22144
(283.06 secs, 98,875,522,800 bytes)
"?: " 

-}
                  
{-

cesta  len m n path = concat [ cesta (len-1) m n ((ii,jj):path) |
                        (ii,jj) <- 
                          [(f1 i, f2 j) | (f1, f2) <- susedne, notElem ((f1 i),(f2 j)) path, f1 i>=0, f1 i < m, f2 j>=0, f2 j <n]
                  ]
                  where (i,j) = head path

-}                  
                  
                  {-
cesta  len m n path = concat [ cesta (len-1) m n ((ii,jj):path) |
                        (ii,jj) <- 
                            --filter (\(x,y)->x>=0 && y>=0 && x<m && y<n && notElem (x,y) path) 
                              --filter (\(x,y)->notElem (x,y) path) 
                              [(f1 i, f2 j) | (f1, f2) <- susedne, notElem ((f1 i),(f2 j)) path, f1 i>=0, f1 i < m, f2 j>=0, f2 j <n]
                  ]
                  where (i,j) = head path
                  
                  
                  -}