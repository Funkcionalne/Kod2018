module Main where

import qualified Faktorial as F
import Test.HUnit
import System.Random

main = do
	g <- getStdGen
	runTestTT $ 
		TestList [ 
			TestList [ -- prva fcia
				TestCase $ assertEqual "fact 0" 1 ( F.fact 0 )
				,
				TestCase $ assertEqual "fact 5" 120 ( F.fact 5 )
				,
				let lst = map (`mod` 20) $ take 10 (randoms g :: [Integer]) in
					TestList[
					   TestCase $ assertEqual ("fact:" ++ (show x)) (myfact x) (F.fact x) | x <- lst]
				]
		]

-- riesenie tutora
myfact n = myfact' n 1

myfact' 0 acc = acc
myfact' n acc = myfact' (n-1) (n*acc)
