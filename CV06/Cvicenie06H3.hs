module Cvicenie06H3 where
import Data.List

divByZero = 5 `div` 0

recursion x = recursion x

zoznam :: Int -> [Int]
zoznam x = x:(zoznam (x+1))

--foo :: a -> b -> b
foo 0 _ = []
foo x y = y

naturals = [1..]
naturals' = zoznam 1
naturals'' = iterate (+1) 1
naturals''' = 1:[n+1 |n <- naturals''']

evens = [0,2..]
evens' = iterate (+2) 0
evens'' = 0:[n+2 |n <- evens'']
evens''' = map (*2) (0:naturals)
evens'''' = zipWith (\x -> \y -> x+y) naturals naturals
evens''''' = filter (even) naturals

odds = [1,3..]
odds' = iterate (+2) 1
odds'' = map (\x -> 2*x+1) naturals

pwrs2 = map (\x -> 2^x) naturals
pwrs2' = map (2^) naturals
pwrs2'' = 1:[2^n | n <- naturals]
-- pwrs2''' = 2:[n*n | n <- pwrs2'''] 
pwrs2''' = 1:[2*n | n <- pwrs2''']

oneByOne z1 z2 = (take 1 z1) ++ (take 1 z2) ++ (oneByOne (drop 1 z1) (drop 1 z2))  

integers = 0:( oneByOne (map negate naturals) naturals)

racionals = [(fromInteger p )/ (fromInteger q) | q <- naturals, p <- integers]
racionals' = [(fromInteger (s-q) )/ (fromInteger q) | s <- [1..], q <- [1..s]]
racionals'' = [((s-q), q) | s <- [1..], q <- [1..s]]
racionals''' = [((s-q), q) | s <- [1..], q <- [1..s], (gcd (s-q) q) == 1]

merge2 b@(x:xs) a@(y:ys)| x<y = x:(merge2 xs a)
						| otherwise = y:(merge2 b ys)
						
megaMerge xss =  min:(megaMerge (map (dropWhile (== min)) xss))
	where min = (minimum (map head xss))

