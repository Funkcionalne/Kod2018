data Exp = ICon Int
  | Var String
  | Add Exp Exp
  | Sub Exp Exp
  | UMin Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pwr Exp Exp
  | Ln Exp
    deriving (Eq, Ord, Read, Show)
 
-- rozcvicka
{-
Napíšte do modulu ZmazDuplikaty funkciu zmazDuplikaty
module ZmazDuplikaty where
zmazDuplikaty :: [Int] -> [Int]
zmazDuplikaty [] = []
zmazDuplikaty xs = ...
ktorá zo zoznamu xs odstráni všetky duplikáty. 
V riešení nemôžete použiť Data.List, všetky funkcie si musíte urobiť sami.
Príklad:
zmazDuplikaty [1,2,1,2,1,3,1,2,3] vráti [1,2,3]

--hruba sila
zmazDuplikaty :: Ord a => [a] -> [a]
zmazDuplikaty = vyhodRovnake . utried

utried :: Ord a => [a] -> [a]
utried [] = []
utried (x:xs) = utried [xm|xm<-xs,xm<=x] ++ [x] ++ utried [xv|xv<-xs, xv>x]

vyhodRovnake :: Ord a => [a] -> [a]
vyhodRovnake (x1:x2:xs) = 
  if x1 == x2 then vyhodRovnake (x2:xs) else x1:(vyhodRovnake (x2:xs))
vyhodRovnake x = x

-- hruba sila inak
zmazDuplikaty :: [Int] -> [Int]
zmazDuplikaty [] = []
zmazDuplikaty xs = pom xs []
 
pom :: [Int] -> [Int] -> [Int]
pom [] final = final
pom (x:xs) final 	| (length [y | y <- final, y == x]) == 0 = pom xs (x:final)
					| otherwise = pom xs final

-- pekne
zmazDuplikaty :: [Int] -> [Int]
zmazDuplikaty [] = []
zmazDuplikaty (x:xs) = x : zmazDuplikaty (odstran x xs)
 
odstran :: Int -> [Int] -> [Int]
odstran y [] = []
odstran y (x:xs) | y == x    = [] ++ (odstran y xs)   -- nebolo by lepsie len odstran y xs?
                 | otherwise = x:(odstran y xs)

--elegancia
zmazDuplikaty :: [Int] -> [Int]
zmazDuplikaty [] = []
zmazDuplikaty xs = foldr (\x y -> if (elem x y) then y else x:y) [] xs

--a aj bez foldr
zmazDuplikaty :: [Int] -> [Int]
zmazDuplikaty [] = []
zmazDuplikaty (x:xs) = (if elem x xs then [] else [x]) ++ zmazDuplikaty xs
-}
--
-- pocet premennych vo vyraze, ich zoznam

{- riesenie 1
zoznamVar :: Exp -> [Exp]
zoznamVar = zoznamVar' []

zoznamVar' :: [Exp] -> Exp -> [Exp]
zoznamVar' z (ICon _) = z
zoznamVar' z (Var v) =  zmazDuplikaty ((Var v):z)
zoznamVar' z (Add e1 e2) = zmazDuplikaty (zoznamVar' z e1 ++ zoznamVar' z e2)
zoznamVar' z (Sub e1 e2) = zmazDuplikaty (zoznamVar' z e1 ++ zoznamVar' z e2)
-}
v1 = ICon 10
v2 = Var "x"
v3 = Add v1 v2
v4 = Sub v3 v3
v5 = Add v3 v4
--v5 = Mul v3 v4
v6 = Div v4 v5

-- riesenie 2
zoznamVar :: Exp -> [Exp]
zoznamVar = zmazDuplikaty . zoznamVar'
  where
    zoznamVar' (ICon _) = []
    zoznamVar' (Var v) = [Var v]
    zoznamVar' (Add e1 e2) = zoznamVar' e1 ++ zoznamVar' e2
    zoznamVar' (Sub e1 e2) = zoznamVar' e1 ++ zoznamVar' e2
    zoznamVar' (Mul e1 e2) = zoznamVar' e1 ++ zoznamVar' e2
    zoznamVar' (Div e1 e2) = zoznamVar' e1 ++ zoznamVar' e2
    zoznamVar' (Pwr e1 e2) = zoznamVar' e1 ++ zoznamVar' e2
    zoznamVar' (UMin e) = zoznamVar' e
    zoznamVar' (Ln e) = zoznamVar' e

-- dosadenie konstanty za premenne / vyhodnotenie vyrazu - derivacia v bode
--       co  za premennu  v com  vysledok
dosad :: Exp -> String -> Exp -> Exp
dosad co s (Var v)= if v == s then co else (Var v)
dosad co s (ICon c) = ICon c
dosad co s (Add e1 e2) = Add (dosad co s e1) (dosad co s e2)
dosad co s (Sub e1 e2) = Sub (dosad co s e1) (dosad co s e2)

-- pocet binarnych/unarnych funkcii vo vyraze (ich zoznamy)
-- "hlbka" vyrazu
hlbka :: Exp -> Int
hlbka (Add e1 e2) = 1 + max (hlbka e1) (hlbka e2)
hlbka (Sub e1 e2) = 1 + max (hlbka e1) (hlbka e2)
hlbka _ = 1

-- "sirka" vyrazu
sirka :: Exp -> Int
sirka v = sirka' 1 [v]

sirka' :: Int -> [Exp] -> Int
sirka' s [] = s
sirka' s vs = sirka' ns nvs
  where nvs = concat (map nasledovnik vs)
        ns = max s (length nvs)

nasledovnik :: Exp -> [Exp]
nasledovnik (ICon _) = []
nasledovnik (Var _) = []
nasledovnik (UMin e) = [e]
nasledovnik (Ln e) = [e]
nasledovnik (Add e1 e2) = [e1, e2]
nasledovnik (Sub e1 e2) = [e1, e2]
nasledovnik (Mul e1 e2) = [e1, e2]
nasledovnik (Div e1 e2) = [e1, e2]

-- aky problem by bol so substituciou za premennu? bol by nejaky? kedy by bol?
-- modifikacia na vseobecny strom Tree a = Empty | Branch a [Tree a]
-- ako sa zmeni sirka hlbka?
-- preorder, inorder, postorder
-- aky hlboky zasobnik potrebujeme na vyhodnotenie vyrazu?