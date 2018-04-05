module Cvicenie06 where

-- http://dai.fmph.uniba.sk/courses/PARA/Prednasky/funkcionalne4.pdf

divbyzero  = 5/0
loop  = loop
---
foo 0  _  = 0
foo n  x  = x/2

-- definujte zoznam prirodzenych cisel, roznymi sposobmi
naturals  = [1..]
naturals' = iterate (+1) 1
naturals'' = from 1
from:: Int -> [Int]
from n = n:(from (n+1))

ones = 1:ones
naturals''' = scanr (\x -> \y -> x+y) 1 ones

-- definujte zoznam parnych cisel, roznymi sposobmi
evens  = map (*2) naturals
evens' = filter (\x -> x `mod` 2 == 0 ) naturals
evens''= [2,4..]
evens''' = 2 : (map (+2) evens''')
evensfrom:: Int -> [Int]
evensfrom n = n:(evensfrom (n+2))
-- definujte zoznam neparnych cisel, roznymi sposobmi
odds  = map (+(-1)) evens
odds' = [1,3..]
odds''= map (\x -> x*2 -1) naturals
-- definujte zoznam mocnin 2, 2^n, roznymi sposobmi
powers2  = [2^n | n <- naturals]
powers2' = map (\n -> 2^n) naturals
powers2'' = map (2^) naturals
-- definujte zoznam mocnin 3, 3^n, roznymi sposobmi
powers3  = 1:(map (*3) powers3)

-- definujte zoznam vsetkych celych cisel
integers  = 0:oneByOne naturals (map negate naturals) 
--integers' = 
-- definujte funkciu, ktora zluci dva (nekonecne) zoznamy, jeden-po-jednom prvku
oneByOne (x:xs) (y:ys) = x:y:(oneByOne xs ys)

-- definujte funkciu, ktora zluci dva (nekonecne) utriedene zoznamy, vysledkom musi byt utriedeny zoznam
--merge (x:xs) (y:ys) = concat [ [n,m] | n <- xs, m <- ys] zle
merge a@(x:xs) b@(y:ys) | x < y = x:(merge xs b)
                        | x > y = y:(merge a ys)
                        | otherwise = x:y:merge xs ys                      


-- definujte usporiany zoznam cisel tvaru 2^n alebo 3^n alebo 5^n

-- definujte nekonecny zoznam racionalnych cisel
rationals = undefined

gcd 0 0 = undefined
gcd a b = gcd' (abs a) (abs b)
    where gcd' a 0 = a
          gcd' a b = gcd' b (a `rem` b)
          
-- definujte zoznam dvojic prirodzenych cisel, ale len nesudelitelnych...       

-- definujte zoznam vsetkych realnych cisel
reals = undefined
   
-- definujte kartezsky sucin dvoch zoznamov
cp  :: [[t]] -> [[t]]
cp = undefined   
   
-- cp [ [1,2,3], [10,11], [-1,-2,-3]]

-- problem: cp [naturals,naturals]

-- riesenie aspon pre dve nekonecne mnozniny

-- definujte riesenie pre dva nekonecne zoznamy

-- definujte riesenie pre lubovolny konecny pocet nekonecnych zoznamov
   
-- definujte riesenie pre lubovolny nekonecny pocet nekonecnych zoznamov
