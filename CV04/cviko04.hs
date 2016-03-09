
--derivovac
-----------
data Exp = ICon Int
  | Var String
  | Add Exp Exp
  | Sub Exp Exp
  | UMin Exp  | Mul Exp Exp
  | Div Exp Exp
  | Pwr Exp Exp
  | Ln Exp
      deriving (Eq, Ord, Read, Show)

derive :: Exp -> String -> Exp
derive (ICon _) _ = ICon 0
derive (Var x) y | x == y = ICon 1
                        | otherwise = ICon 0
derive (Add e1 e2) x = urobAdd (derive e1 x) (derive e2 x)
derive (Sub e1 e2) x = urobSub (derive e1 x) (derive e2 x)
derive (UMin e) x = UMin (derive e x)
urobAdd :: Exp -> Exp -> Exp
urobAdd (ICon x) (ICon y) = ICon (x+y)
urobAdd e1 e2
  | e1 == (ICon 0) = e2
  | e2 == (ICon 0) = e1
  | otherwise = Add e1 e2
urobSub :: Exp -> Exp -> Exp
urobSub (ICon x) (ICon y) = ICon (x-y)
urobSub e1 e2
  | e1 == (ICon 0) = UMin e2
  | e2 == (ICon 0) = e1
  | e1 == e2 = ICon 0
  | otherwise = Sub e1 e2

-----------------
expToStr :: Exp -> String
expToStr (ICon x) = show x
expToStr (Var x) = x
--expToStr (Add e1 e2) = "(" ++ expToStr e1 ++ " + " ++ expToStr e2 ++ ")"
--expToStr (Sub e1 e2) = "(" ++ expToStr e1 ++ " - " ++ expToStr e2 ++ ")"
expToStr (Add e1 e2) = expToStr e1 ++ " + " ++ expToStr e2 
expToStr (Sub e1 e2) = expToStr e1 ++ " - (" ++ expToStr e2 ++ ")"
expToStr (UMin e) = "-(" ++ expToStr e ++ ")"

d v p = putStrLn (expToStr (derive v p))
-----------------
c1 = ICon 1
cm1 = ICon (-1)
c2 = ICon 2
c3 = ICon 3
vx = Var "x"
vy = Var "y"
e3 = Add c1 c2  -- 2+3
e4 = Add c3 vx  -- 3+x 
e5 = Add c2 vy  -- 2+y
e6 = Add e3 e4  -- (2+3)+(3+x)
e7 = Add e4 e3  -- (3+x)+(2+3)
e8 = Add e4 e4  -- (3+x)+(2+y)
e9 = Sub e3 e6  -- 
e10 = Sub e9 e9 -- 0

----- rozcvicka
{-
Do modulu 
module Povkladaj where
dopíšte funkciu
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
-}
