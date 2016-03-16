-- 2. príklad
-- Michal Smolík
cart :: [a] -> [b] -> [(a,b)]
cart xs [] = []
cart [] ys = []
cart (x:xs) ys = map (\y -> (x,y)) ys ++ cart xs ys
 
-- [x|xs<-xss, x<-xs, odd x]
riesenie1 :: [[Int]] -> [Int]
riesenie1 xss = filter odd $ concat xss
 
--[(x,y)|x<-xs,p x, y<-ys]
riesenie2 :: (a -> Bool) -> [a] -> [b] -> [(a,b)]
riesenie2 p xs ys = cart (filter p xs) ys

-- 3 príklad
{- Michal Smolík
a) reverse (xs ++ ys) = reverse ys ++ reverse xs
Urobíme indukciu na dĺžke xs:
1° reverse([]++ys) = reverse ys = reverse ys ++ reverse []
2° Predpoklad: pre xs a ys platí reverse (xs ++ ys) = reverse ys ++ reverse xs
   ideme dokázať že to platí aj pre x:xs
 
   reverse (x:xs ++ ys) = reverse (xs ++ ys) ++ [x] =
   = (reverse ys ++ reverse xs) ++ [x] = reverse ys ++ (reverse xs ++ [x])=
   = reverse ys ++ reverse (x:xs)
 
b) foldr f a (xs ++ ys) = foldr f (foldr f a ys) xs
Indukcia na dĺžke xs:
1° foldr f a (xs ++ []) = foldr f a xs = foldr f (foldr f a []) xs
 
2° nech pre každé xs a ys platí foldr f a (xs ++ ys) = foldr f (foldr f a ys) xs
   dokážeme že to platí aj pre x:xs:
 
   foldr f a (x:xs ++ ys) = f x (foldr f a (xs ++ ys)) =
   = f x (foldr f (foldr f a ys) xs) = foldr f (foldr f a ys) x:xs
-}

--6. príklad 
{-
Sa dal vyskušaním všetkých permutácií n funkcií. Tak ste to mali všetci, kto ste ju odovzdali
Ale n! rychlo rastie...
Malo by ist aj nejako lepsie... Vyuzit, ze kaza funkcia ma prave jednu nenulovu hodnotu a 
aplikuje sa to na jedinu hodnotu.
-}