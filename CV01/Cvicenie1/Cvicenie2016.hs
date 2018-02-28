module Cvicenie where

-- zakladne typy v haskelly:
-- Int (32/64bit), Integer, Double, Bool, Char, String
-- pravdivostna hodnota: True, False

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

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

zoznam = [1,2,3,4,5,6]

zoznam2 = 1 : (2 : (3 : []))

zoznam3 = [1,3..100]

dlzkaZoznamu :: [Integer] -> Integer
dlzkaZoznamu [] = 0
dlzkaZoznamu (_:xs) = 1 + dlzkaZoznamu xs

delitele :: Integer -> [Integer]
delitele n = d n [1..n]
    where d :: Integer -> [Integer] -> [Integer]
          d _ []     = []
          d n (x:xs) | mod n x == 0 = x : d n xs
                     | otherwise    = d n xs

delitele' :: Integer -> [Integer]
delitele' n = [x | x <- [1..n], mod n x == 0]

kartezkySucin :: [Integer] -> [Integer] -> [(Integer, Integer)]
kartezkySucin xs ys = [(x, y) | x <- xs, y <- ys]

jeVZozname :: Integer -> [Integer] -> Bool
jeVZozname _ []                 = False
jeVZozname n (x:xs) | x == n    = True
                    | otherwise = jeVZozname n xs

spojZoznamy :: [Integer] -> [Integer] -> [Integer]
spojZoznamy [] ys     = ys
spojZoznamy (x:xs) ys = x : spojZoznamy xs ys