module MII where -- definicia modulu, treba pri testovaci dodrzat

import Data.Char
import Test.QuickCheck

-- typy Int, Integer, Bool, Char, String
-- type String = [Char]

fib :: Integer -> Integer
fib 0  = 1
fib 1  = 1
fib n  = fib(n-1) + fib(n-2)

fib' :: Integer -> Integer
fib' n  | n <= 1 =   1
        | otherwise = fib'(n-1) + fib'(n-2)

fib'' :: Integer -> Integer
fib'' n  = if n <= 1 then 1 else fib''(n-1) + fib''(n-2)

fib'''' :: Integer -> Integer
fib'''' n = f n 0 1
    where f 0 a b = b
          f n a b = f (n-1) b (a+b)

qch1 = quickCheck((\n ->  (n>=0 && n < 13)==> (fib n == fib'''' n)))

sum2 :: [Integer] -> [Integer] -> [Integer]
sum2 [] []  = []
sum2 (x:xs) (y:ys) = (x+y : sum2 xs ys)
sum2 [] _   = []
sum2 _ []   = []

sum2' :: [Integer] -> [Integer] -> [Integer]
sum2' xs ys = [ z+w | z <-xs, w <-ys]

sum2'' :: [Integer] -> [Integer] -> [Integer]
sum2''  xs ys = [  let pom = xs!!i in pom + ys!!i | i <- [0..length xs-1],  i < length ys ]

-- delitel 24 = [1,2,3,4,6,8,12]
delitele :: Integer -> [Integer]
delitele n = [ x | x <- [1..(n-1)], n `mod` x == 0]

-- int2String 6 = "110"

int2String  :: Int -> String -- [Char]
int2String 0 = ""
--int2String 0 = "0"
--int2String 1 = "1"
int2String n = int2String (n `div` 2) ++ [pom]
      where pom = chr(n `mod` 2 + 48)

comb n k = product [1..n]/(product [1..k] * product [1..n-k])
