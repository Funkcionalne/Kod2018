module Cvicenie07 where
import Unsafe.Coerce 

-- robili sme na minulom cviceni
merge2 b@(x:xs) a@(y:ys)| x<y = x:(merge2 xs a)
                        | x>y = y:(merge2 b ys)
                        | otherwise = x:(merge2 xs ys)
                        
hamming23   = 1: merge2 [2*x | x<-hamming23] [3*x | x<-hamming23]

hamming235   = 1: merge2 [2*x | x<-hamming235] (merge2 [3*x | x<-hamming235] [5*x | x<-hamming235])

---
kolko2 0=0
kolko2 1=1
kolko2 x=1+kolko2 (x `div` 2)

kolko23 0=0
kolko23 x=kolko2 x + kolko23 (x`div` 3)

kolko235 0=0
kolko235 x=kolko23 x + kolko235 (x`div` 5)

kolko2357 0=0
kolko2357 x=kolko235 x +kolko2357 (x`div` 7)

find x= [(a,kolko2357 a),(b,kolko2357 b)]where (a,b)=findh x 1

findh x y| kolko2357 y < x = findh x (y*10)
         | otherwise = findb x (y`div`10) y

findb x min max | abs(min-max)<2 = (min,max)
                |   kd<x = findb x d max
                |   kd>x = findb x min d
                |   kd==x = findb x min d 
                where d = (min+max)`div` 2
                      kd = kolko2357 d

--1000. prvok   385875
--10000. prvok  63221760000
--100000. prvok 123093144973968750000
--
--Main> find 1000
--[(385874,999),(385875,1000)]
--Main> find 10000
--[(63221759999,9999),(63221760000,10000)]
--Main> find 100000
--[(123093144973968749999,99999),(123093144973968750000,100000)]
--

fib = 1 : 1 : [ a+b | (a,b) <- zip fib (tail fib) ]

fibo@(1:tfib) = 1 : 1 : [ a+b | (a,b) <- zip fibo tfib ]

primes = sieve [ 2.. ] where
            sieve (p:x) = p : sieve [ n | n<-x, n `mod` p > 0 ]


primes'     :: [Int]
primes'     = map head (iterate sieve [2 ..])
sieve       :: [Int] -> [Int]
sieve (p:ps) = [x | x <- ps, (x `mod` p) /= 0]


pascal :: [[Int]]
pascal = [1] : [[x+y | (x,y) <- zip ([0]++r) (r++[0])] | r <- pascal]

-----------------------------------------------

-- zdroj http://techtipshoge.blogspot.sk/2011/06/church-number-with-ski-combinators.html
-- http://www.angelfire.com/tx4/cus/combinator/birds.html

-- SKI combinator  
-- ?x.x  
i = \x -> x

-- ?xy.x  
k = \x -> \y -> x

-- ?xyz.x z (y z)  
s = \x -> \y -> \z -> x z (y z)

-- inak to je church's one
apply'   = s (s k)                            -- ?xy.x y 
apply''  = (s ( (s (k s)) ((s (k k) i)) )) (k i)
apply'''  = i

-- Church's numeral  
-- ?fx.x
zero  = k i  
-- ?fx.(f x)
one   = i  
one'  = (s ( (s (k s)) ((s (k k) i)) )) (k i)
two   = (s (s (k s) k)) i  
three = (s (s (k s) k)) (s (s (k s) k) i)  
four  = (s (s (k s) k)) ((s (s (k s) k)) (s (s (k s) k) i))  


-- For assertion only  
type Church = (Int -> Int) -> Int -> Int  
  
-- integer -> church  
church n = \f -> \x -> iterate f x !! n  

ch_add = \m -> \n -> \f -> \x -> m f (n f x)     

-- one  
ch_one = \f -> \x -> f x  
  
-- n++  
ch_inc = \n -> \f -> \x -> f (n f x)  
  
-- m*n  
ch_mult = \m -> \n -> \f -> \x -> m (n f) x  
  
-- m^n  
ch_expt = \m -> \n -> n (ch_mult m) one  
   

unChurch n = n (+1) (0)  
            

a = church 2  
b = church 10   
  
main = do print $ unChurch $ ch_add a b  
          print $ unChurch $ ch_inc a  
          print $ unChurch $ ch_mult a b  
          print $ unChurch $ ch_expt a b 
          print $ unChurch $ i a
          print $ unChurch $ k a b
          print $ unChurch zero  
          print $ unChurch one  
          print $ unChurch one'
          print $ unChurch two  
          print $ unChurch three  
          print $ unChurch four  
          print $ unsafeApply apply' (+1) 10   
          print $ unsafeApply apply'' (^2) 10   
          print $ unsafeApply apply''' (subtract 99) 10  
          
unsafeApply n a b = unsafeApply' (unsafeCoerce n) a b            
      where unsafeApply' :: Church -> (Int -> Int) -> Int -> Int  
            unsafeApply' n a b = n a b  
          