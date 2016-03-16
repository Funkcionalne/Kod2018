{-

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

a) reverse (xs ++ ys) = reverse ys ++ reverse xs

N = length xs
M = length ys

pre N = 0, M = 0
reverse ([] ++ []) = reverse [] ++ reverse []
reverse [] = [] ++ []
[] = []

pre N = 0, vseobecne M
reverse ([] ++ ys) = reverse ys ++ reverse []
reverse ys = reverse ys ++ []
reverse ys = reverse ys

pre N + 1, vseobecne M
reverse (x:xs ++ ys) =                      
= (reverse (xs ++ ys)) ++ [x]               - 2. pravidlo reverse
= (reverse ys ++ reverse xs) ++ [x]         - IP
= reverse ys ++ (reverse xs ++ [x])
= reverse ys ++ reverse x:xs                - 2. pravidlo reverse spatne


--------------------------------------------------------------------


foldr :: (a - > b -> b) -> b -> [a] -> b
foldr f a [] = a
foldr f a (x:xs) = f x (foldr f a xs)

b) foldr f a (xs ++ ys) = foldr f (foldr f a ys) xs

N = length xs
M = length ys

pre N = 0, M = 0
foldr f a ([] ++ []) = foldr f (foldr f a []) []       - 1. pravidlo foldr
foldr f a [] = foldr f a []

pre N = 0, vseobecne M
foldr f a ([] ++ ys) = foldr f (foldr f a ys) []        - 1. pravidlo foldr na vonkajsi foldr
foldr f a ys = foldr f a ys

pre N + 1, vseobecne M
foldr f a (x:xs ++ ys) =                    
= f x (foldr f a (xs ++ ys))                - 2. pravidlo foldr
= f x (foldr f (foldr f a ys) xs)           - IP
= foldr f (foldr f a ys) x:xs               - 2. pravidlo foldr spatne

-}