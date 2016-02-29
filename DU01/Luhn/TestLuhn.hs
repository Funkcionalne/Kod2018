module Main where

import qualified Luhn as F
import Test.HUnit
import System.Random

main = do
	g <- getStdGen
	runTestTT $ 
		TestList [ 
			TestList [ 
				TestCase $ assertEqual "F.cardnumber 49927398716" True  (F.cardnumber 49927398716),
				TestCase $ assertEqual "F.cardnumber 79927398710" False (F.cardnumber 79927398710),
				TestCase $ assertEqual "F.cardnumber 79927398711" False (F.cardnumber 79927398711),
				TestCase $ assertEqual "F.cardnumber 79927398712" False (F.cardnumber 79927398712),
				TestCase $ assertEqual "F.cardnumber 79927398713" True  (F.cardnumber 79927398713),
				TestCase $ assertEqual "F.cardnumber 79927398714" False (F.cardnumber 79927398714),
				TestCase $ assertEqual "F.cardnumber 79927398715" False (F.cardnumber 79927398715),
				TestCase $ assertEqual "F.cardnumber 79927398716" False (F.cardnumber 79927398716),
				TestCase $ assertEqual "F.cardnumber 79927398717" False (F.cardnumber 79927398717),
				TestCase $ assertEqual "F.cardnumber 79927398718" False (F.cardnumber 79927398718),
				TestCase $ assertEqual "F.cardnumber 79927398719" False (F.cardnumber 79927398719)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			],
			TestList [	
					let lst = map (`mod` 10) $ take 16 (randoms g :: [Integer])in
					let cn = foldr (\x -> \y -> 10*x+y) 0 lst
					in
					TestCase $ assertEqual ("F.cardnumber:" ++ (show cn)) (mycardnumber cn) (F.cardnumber cn)
			]      
		]

		
-- Get the last digit from a number
lastDigit :: Integer -> Integer
-- lastDigit = undefined
-- riesenie studenta
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
-- dropLastDigit = undefined
-- riesenie studenta
dropLastDigit n = n `div` 10

-- n .. 1
toRevDigits :: Integer -> [Integer]
-- toRevDigits = undefined
-- riesenie studenta
toRevDigits 0 = []
toRevDigits n = n `mod` 10 : toRevDigits (n `div` 10)

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther = undefined
-- riesenie studenta
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x:(y+y):doubleEveryOther xs

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
-- sumDigits = undefined
-- riesenie studenta
cifsum	0	= 0
cifsum	x	= cifsum (x `div` 10) + (x `mod` 10)
sumDigits xs = sum $ map cifsum xs
  
-- Validate a credit card number using the above functions.
mycardnumber :: Integer -> Bool
mycardnumber x	= (sumDigits $ doubleEveryOther $ toRevDigits x) `mod` 10 == 0
