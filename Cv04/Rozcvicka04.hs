module Rozcvicka04 where

{-
Do modulu dopíšte funkciu
povkladaj :: a -> [a] -> [[a]]
povkladaj x ys =
ktorá vytvorí zoznam všetkých možností ako vsunú x medzi prvky ys
Príklad
povkladaj 1 [] vráti [[1]]
povkladaj 0 [1,2,3] vráti [[0,1,2,3],[1,0,2,3],[1,2,0,3],[1,2,3,0]]
najprv vloží pred prvý, potom pred druhý atï a na koniec za posledný.

vase riesenia

priamociare
povkladaj :: a -> [a] -> [[a]]
povkladaj x xs = [ take n xs  ++ [x] ++ drop n xs | n <- [0..(length xs)] ]

s foldr
povkladaj :: a -> [a] -> [[a]]
povkladaj x ys = foldr (\i -> \y -> ((take i ys)++[x]++(drop i ys)):y ) [] [0..(length ys)]

trocha zlozitejsie
posuvaj :: a -> [a] -> [[a]]
posuvaj x ys = map (\q -> insertAt q x ys) [0..(length ys)]

insertAt :: Int -> a -> [a] -> [a] 
insertAt z y xs = as ++ (y:bs)
                  where (as,bs) = splitAt z xs

este zlozitejsie
povsuvaj :: a -> [a] -> [[a]]
povsuvaj x ys = povsuvaj2 (length ys) x ys

povsuvaj2 :: Int -> a -> [a] -> [[a]]
povsuvaj2 0 x ys = [fn 0 x ys]
povsuvaj2 n x ys = (povsuvaj2 (n-1) x ys) ++ [(fn n x ys)]

fn n x ys = (take n (ys)) ++ [x] ++ (drop n (ys))
  
ine
povsuvaj :: a -> [a] -> [[a]]
povsuvaj x ys = [ins ix x ys | ix <- [0..(length ys)] ] 

ins :: Int -> a -> [a] -> [a]
ins 0 x ys = x:ys
ins ix x (y:ys) = y:(ins (ix-1) x ys)

sofistikovane
import Data.List
posuvaj :: (Eq a) => a -> [a] -> [[a]]
posuvaj x ys = filter (\xs -> (delete x xs) == ys) (permutations (x:ys))

a este ine
povkladaj :: a -> [a] -> [[a]]
povkladaj a b = pom [] b a

     --pred   po    vloz  vysledok
pom :: [a] -> [a] -> a -> [[a]]
pom a [] x = [(a ++ [x])]
pom a b x = [(a ++ [x] ++ b)] ++ (pom (a ++ [(head b)]) (tail b) x)

-- moje riesenie

povkladaj 	     	:: t -> [t] -> [[t]]
povkladaj x []  	= [[x]]
povkladaj x (y:ys) = (x:y:ys) : map (y:) (povkladaj x ys)


-}
