module Cifry where

cifry	:: Integer -> [Integer]			
cifry 0 = []
cifry n = cifry (n `div` 10) ++ [n `mod` 10]

cifryR	:: Integer -> [Integer]			
cifryR 0 = []
cifryR n = (n `mod` 10):cifryR (n `div` 10)
