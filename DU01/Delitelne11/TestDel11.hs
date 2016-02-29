module Main where

import qualified Del11 as F
import Test.HUnit
import System.Random

main = do
	g <- getStdGen
	runTestTT $  
		TestList [ 
			TestList [ -- prva fcia
				TestCase $ assertEqual "delitelne11 121" True ( F.delitelne11 121 )
				,
				TestCase $ assertEqual "delitelne11 122" False ( F.delitelne11 122 )
				,
				let lst = map (`mod` 200000000) $ take 1000 (randoms g :: [Integer]) in
					TestList[
					   TestCase $ assertEqual ("delitelne11:" ++ (show x)) (x `mod` 11 == 0) (F.delitelne11 x) | x <- lst]
				]
		]
