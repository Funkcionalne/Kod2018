module Primes where

primes  :: [Integer]
primes = 0:sieve [2..]  where 
             sieve (0:xs) = 0 : sieve xs
             sieve (x:xs) = x : sieve [ if y `mod` x == 0 then 0 else y | y<-xs]
             sieve [] = []
             