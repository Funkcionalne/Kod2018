module Cvicenie1 where

-- zakladne typy v haskelly
-- Int, Integer, Bool, Double, Char, String

cislo1 :: Int
cislo1 = 1

cislo2 :: Integer
cislo2 = 354351303843864310381063810

realneCislo :: Double
realneCislo = 3.1415

pravda :: Bool
pravda = True

znak :: Char
znak = 'A'

retazec :: String
retazec = "Ahoj svet!"

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

nNadK :: Integer -> Integer -> Integer
nNadK n k = div (fact n) (fact k * fact (n - k))

fact' :: Integer -> Integer
fact' 0 = 1
fact' n | n > 0     = n * fact' (n-1)
        | otherwise = 0

factIf :: Integer -> Integer
factIf 0 = 1
factIf n = if n > 0 then n * factIf (n-1)
                    else 0

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' :: Integer -> Integer
fib' n = fibAc 0 1 n
    where fibAc :: Integer -> Integer -> Integer -> Integer
          fibAc a _ 0 = a
          fibAc a b n = fibAc b (a+b) (n-1)

zoznam :: [Integer]
zoznam = [1,2,3,4,5,6]

zoznamInak :: [Integer]
zoznamInak = 1 : 2 : 3 : 4 : 5 : 6 : []

zoznamEsteInak :: [Integer]
zoznamEsteInak = [1,3..100]

dlzkaZoznamu :: [Integer] -> Integer
dlzkaZoznamu []     = 0
dlzkaZoznamu (_:xs) = 1 + dlzkaZoznamu xs

delitele :: Integer -> [Integer]
delitele n = vD n [1..n]
    where vD :: Integer -> [Integer] -> [Integer]
          vD _ []     = []
          vD n (x:xs) | mod n x == 0 = x : vD n xs
                      | otherwise    = vD n xs

-- [fx(x) for x in range(8) if mod(x, 2) == 0] Python
-- [fx x | x <- [0..7], mod x 2 == 0] Haskell

delitele' :: Integer -> [Integer]
delitele' n = [x | x <- [1..n], mod n x == 0]
-- {x | x in {1, .. , n} and n|x} zhruba matematicky