module Main where

import qualified Tribonacci as F
import Test.HUnit
import System.Random

main = do
	g <- getStdGen
	runTestTT $ 
		TestList [ 
			TestList [ -- prva fcia
				TestCase $ assertEqual "trib 0" 1 ( F.trib 0 )
				,
				TestCase $ assertEqual "trib 5" 9 ( F.trib 5 )
				,
				let lst = map (`mod` 20) $ take 10 (randoms g :: [Integer]) in
					TestList[
					   TestCase $ assertEqual ("trib:" ++ (show x)) (mytrib x) (F.trib x) | x <- lst]
				]
		]

-- riesenie tutora
mytrib 0 = 1
mytrib 1 = 1
mytrib 2 = 1
mytrib n = mytrib (n-1) + mytrib(n-2) +mytrib (n-3)

