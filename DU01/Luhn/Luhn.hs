module Luhn where

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
cardnumber :: Integer -> Bool
cardnumber x	= (sumDigits $ doubleEveryOther $ toRevDigits x) `mod` 10 == 0
