module Vypln where

import Data.List
import VyplnTypy

{-
po kontrole riesitelnosti - (parita)
  1. pocet policok na doske musi byt parny (kedze domino pokryje 2 policka)
  2. predstavte si, ze doska by bola cierno biela ako sachovnica.
     Zakazane policka nemozu mat rovnaku farbu, lebo kazde domino zakryje 
	 jedno biele a jedno cierne policko
sa pouzije maly trik.
Najdeme cyklus (asi je ich viacej, ale staci jeden) cez vsetky policka dosky (aj zakazane). 
Zacat (a skoncit mozeme napr. na (P 1 1)).
Ukazeme na priklade
  ries 3 4 (P 2 2) (P 3 4)
  tabulka 3 riadky a 4 stlpce
  [P 1 1,P 1 2,P 1 3,P 1 4,P 2 4,P 3 4,P 3 3,P 2 3,P 2 2,P 3 2,P 3 1,P 2 1]
teraz zrotujeme tak aby prvy prvokbolo zakazane policko
  [P 2 2,P 3 2,P 3 1,P 2 1,P 1 1,P 1 2,P 1 3,P 1 4,P 2 4,P 3 4,P 3 3,P 2 3]
vyhodime zakazane policka (medzi nimi aj okolo nich musia byt useky  parnej dlzky
  [P 3 2,P 3 1,P 2 1,P 1 1,P 1 2,P 1 3,P 1 4,P 2 4,P 3 3,P 2 3]
a uz staci len poparit susedne policka na domina
  [Vert (P 3 1),Horiz (P 1 1),Vert (P 1 2),Horiz (P 1 4),Horiz (P 2 3)]
  
Specialny pripad je ak mame sliz t.j. 1 riadok, resp 1 stlpec,
vtedy sa nam nepodari urobit cyklus, ale to nevadi, riesenie funguje aj tu
kvoli tomu, ze useky su parnej dlzky.

inspiracia: ELTE Budapest http://lambda.inf.elte.hu/fp/Index.xml
-}
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

---- kontrola riesenia 
kontrolaRiesenia :: Int -> Int -> Pos -> Pos -> [Domino] -> Bool
kontrolaRiesenia m n (P x1 y1) (P x2 y2) ds =
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

kr m n (P x1 y1) (P x2 y2) ds = 
  if null ds == null r then ds
  else
    if kontrolaRiesenia m n (P x1 y1) (P x2 y2) ds then r else []
  where
    r = ries m n (P x1 y1) (P x2 y2) 

-----
r1 = [(Vert (P 1 1)), (Horiz (P 2 2))]
r2 = [(Vert (P 1 1)), (Vert (P 2 2))]

{-
Je daná hracia doska m riadkov a n stĺpcov políčok, na ktorej sú dve políčka zakázané.
Vašou úlohou je zistiť či sa takáto doska dá pokryť dominami veľkosti 1*2 políčok. 
Všetky políčka okrem zakázaných musia byť pokryté dominami. 
Dominá sa nesmú prekrývať a všetky musia byť celé na hracej doske.

Napíšte funkciu ries do modulu Pokri where

-- reprezentacia pozicie policka
data
  Pos = P Int Int
       deriving (Eq, Ord, Show, Read)

-- reprezentacia domina 
-- orientovaneho horizontalne resp. vertikalne,
-- ktoreho lavy horny roj je na pozicii Pos
data
  Domino = Horiz Pos | Vert  Pos 
       deriving (Eq, Show, Read)

ries :: Int -> Int -> Pos -> Pos -> [Domino]
ries m n (P x1 y1) (P x2 y2) = ...

funkcia ries dostane rozmery dosky a pozície dvoch zakázaných políčok. 
Výsledkom je zoznam domín, ktoré tvoria pokrytie. Ak pokrytie neexistuje výsledkom je prázdny zoznam.
-}