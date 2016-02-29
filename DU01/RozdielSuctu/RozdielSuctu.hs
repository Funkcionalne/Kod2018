module RozdielSuctu where

rozdielSuctu :: [Integer] -> Integer
rozdielSuctu  = negate . foldr (-) 0
