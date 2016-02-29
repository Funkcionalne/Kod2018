module Main where

import qualified Hanoi as F
import Test.HUnit
import System.Random

main = do
	g <- getStdGen
	runTestTT $ 
		TestList [ 
			TestList [ -- prva fcia
				let lst = map (`mod` 16) $ take 10 (randoms g :: [Integer]) in
					TestList[
					   TestCase $ assertBool ("F.hanois 4 tycky:" ++ (show x)) 
             ((length (myhanois x ["a", "b", "c", "d"])) >= (length (F.hanois x ["a", "b", "c", "d"])))
             | x <- lst
          ]
          ,
				let lst = map (`mod` 16) $ take 10 (randoms g :: [Integer]) in
					TestList[
					   TestCase $ assertBool ("F.hanois 5 tyciek:" ++ (show x)) 
             ((length (myhanois x ["a", "b", "c", "d", "e"])) >= (length (F.hanois x ["a", "b", "c", "d", "e"])))
             | x <- lst
          ]
          ,
				let lst = map (`mod` 16) $ take 10 (randoms g :: [Integer]) in
					TestList[
					   TestCase $ assertBool ("F.hanois 3 tycky:" ++ (show x)) 
             ((length (myhanois x ["a", "b", "c"])) >= (length (F.hanois x ["a", "b", "c"])))
             | x <- lst
          ]
				]
		]

    
-- riesenie pre tri veze
hanoi :: Integer -> String -> String -> String -> [(String, String)]
hanoi 0 from to aux = []
hanoi n from to aux = hanoi (n-1) from aux to ++ [(from,to)] ++ hanoi (n-1) aux to from

myhanois :: Integer -> [String] -> [(String, String)]
myhanois 0 _  = []
myhanois 1 [from, to] 		= [(from, to)]
myhanois 2 [from, to] 		= undefined
myhanois n [from, to, aux] 	= hanoi n from to aux
myhanois n (from:to:aux:rest) = 		myhanois k (from:aux:to:rest) ++ 
									myhanois (n-k) (from:to:rest) ++ 
									myhanois k (aux:to:from:rest)
        where k = n `div` 2
        
-- length $ myhanois 15 ["a", "b", "c", "d" ]  
