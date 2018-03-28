module Povkladaj where

povkladaj 	     	:: t -> [t] -> [[t]]
povkladaj x []  	= [[x]]
povkladaj x (y:ys) = (x:y:ys) : map (y:) (povkladaj x ys)

{-
povkladaj 	     	:: t -> [t] -> [[t]]
povkladaj x []  	= [[x]]
povkladaj x (y:ys) = (x:y:ys) : map (y:) (povkladaj x ys)
-------------
Do modulu dopφÜte funkciu
povkladaj :: a -> [a] -> [[a]]
povkladaj x ys =
ktorß vytvorφ zoznam vÜetk²ch mo₧nostφ ako vsun·¥ x medzi prvky ys
Prφklad
povkladaj 1 [] vrßti [[1]]
povkladaj 0 [1,2,3] vrßti [[0,1,2,3],[1,0,2,3],[1,2,0,3],[1,2,3,0]]
najprv vlo₧φ pred prv², potom pred druh² at∩ a na koniec za posledn².

vase riesenia

priamociare
povkladaj :: a -> [a] -> [[a]]
povkladaj x xs = [ take n xs  ++ [x] ++ drop n xs | n <- [0..(length xs)] ]

s foldr
povkladaj :: a -> [a] -> [[a]]
povkladaj x ys = foldr (\i -> \y -> ((take i ys)++[x]++(drop i ys)):y ) [] [0..(length ys)]

trocha zlozitejsie
povkladaj :: a -> [a] -> [[a]]
povkladaj x ys = map (\q -> insertAt q x ys) [0..(length ys)]

insertAt :: Int -> a -> [a] -> [a] 
insertAt z y xs = as ++ (y:bs)
                  where (as,bs) = splitAt z xs

este zlozitejsie
  
ine
povkladaj :: a -> [a] -> [[a]]
povkladaj x ys = [ins ix x ys | ix <- [0..(length ys)] ] 

ins :: Int -> a -> [a] -> [a]
ins 0 x ys = x:ys
ins ix x (y:ys) = y:(ins (ix-1) x ys)

sofistikovane  -- nefunguje
import Data.List
povkladaj :: (Eq a) => a -> [a] -> [[a]]
povkladaj x ys = filter (\xs -> (delete x xs) == ys) (permutations (x:ys))

a este ine
povkladaj :: a -> [a] -> [[a]]
povkladaj a b = pom [] b a

     --pred   po    vloz  vysledok
pom :: [a] -> [a] -> a -> [[a]]
pom a [] x = [(a ++ [x])]
pom a b x = [(a ++ [x] ++ b)] ++ (pom (a ++ [(head b)]) (tail b) x)


-}

