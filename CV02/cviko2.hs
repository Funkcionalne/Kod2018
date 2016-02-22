{-
- list-comprehension
- funkcia ako hodnota
- funkcia, ktorá vráti numerickú aproximáciu inverznej funkcie, pre reálnej monotónne funkcie
- kompozícia funkcií, bodková notácia
- kompozícia zoznamu funkcií,  :: [a->a] -> (a->a)
- aplikácia zoznamu funkcií na zoznam argumentov, :: [a->b] -> [a] -> [b]

ALE nie derivaciu, nie odmocninu, nie newtona
-}


delitele :: Int -> [Int]
delitele n = [d | d <- [1..n], n `mod` d == 0]

nsd :: Int -> Int -> Int
nsd  a b = maximum [d | d <- delitele a, b `mod` d == 0]

prvocislo :: Int -> Bool
prvocislo n = delitele n == [1,n]

prvocisla :: Int -> [Int]
prvocisla n = [p | p <- [2..n], prvocislo p]

-- zmensime interval + nejake konverzie medzi cislami
prvocislo' :: Int -> Bool
prvocislo' n = [d | d <- [2..round(sqrt(fromIntegral(n)))], n `mod` d == 0] == []
prvocisla' :: Int -> [Int]
prvocisla' n = [p | p <- [2..n], prvocislo' p]

-- ale ide to aj takto
prvocislaInak :: [Int]
prvocislaInak = 2 : [p | p <- [3..], odd p, prvocislo p]
-- a dokonca takto - kuknite si v klude doma
prvocislaInak' :: [Int]
prvocislaInak' = sito [2..]
  where 
     sito :: [Int] -> [Int]
     sito (p:ks) = p:sito[k | k <- ks, k `mod` p /= 0]
  
-- na prepis list comprehensions sa daju pouzit tieto rovnosti
{-
[x | x<-xs] = xs
[f x | x <- xs] = map f xs
[e | x <- xs, p x,...] = [e | x <- filter p xs, ...]
[e | x <- xs, y <- ys, ...] = concat[[e | y <- ys, ...] | x <- xs]
+ definovanie pomocnych funkcii

napr.
[1 | x <- xs] = map (const 1) xs, kde const 1 = 1
-}

--sekcie, +1,...

-- sum = foldr (+) 0
-- product = foldr (*) 1
-- concat = foldr (++) []
-- and = foldr (&&) True
-- or = 
-- reverse = foldr naKoniec [] where naKoniec x xs = xs ++ [x]
-- takeWhile p = foldr plusAkp [] where plusAkp x xs = if p x then [x] ++ xs else []

-- invNumAprox f y = x   -- |f(x) - y| < epsilon
-- len pre neklesajuce funkcie, pre nerastuce treba pravdepodobne upravit
invNumAprox :: (Float -> Float) -> Float -> Float
invNumAprox f y = bv f y 0 (horna f y) 

-- odhad co najmensieho max x takeho, ze f(x)>y
horna :: (Float -> Float) -> Float -> Float
horna f y = h y
  where
    h x = if f(x) > y then x else h(2*x) 

-- binarne vyhladavanie
bv :: (Float -> Float) -> Float -> Float -> Float -> Float
bv f y dolna horna 
  | abs(fstred - y) < epsilon = stred 
  | fstred > y = bv f y dolna stred
  | otherwise  = bv f y stred horna
    where
      epsilon = 0.01
      stred = (dolna + horna) / 2
      fstred = f stred

myf x = x*x / 10
