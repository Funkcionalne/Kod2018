module Cvicenie07 where
import Unsafe.Coerce 

-- robili sme na minulom cviceni
merge2 b@(x:xs) a@(y:ys)| x<y = x:(merge2 xs a)
                        | x>y = y:(merge2 b ys)
                        | otherwise = x:(merge2 xs ys)
                        
---
kolko2 0=0
kolko2 1=1
kolko2 x=1+kolko2 (x `div` 2)

kolko23 0=0
kolko23 x=kolko2 x + kolko23 (x`div` 3)



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

type Church = (Int -> Int) -> Int -> Int  
  
-- integer -> church  
church n = undefined

-- inak to je church's one
apply'   = s (s k)                            -- ?xy.x y 
apply''  = (s ( (s (k s)) ((s (k k) i)) )) (k i)

-- Church's numeral  
-- ?fx.x
zero  = undefined  
-- ?fx.(f x)
one   = undefined   
one'  = undefined  
two   = undefined  
three = undefined  
four  = undefined  



ch_add = undefined  

-- one  
ch_one = undefined  
  
-- n++  
ch_inc = undefined  
  
-- m*n  
ch_mult = undefined  
  
-- m^n  
ch_expt = undefined  
   

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
          print $ unsafeApply apply'' (subtract 99) 10  
          
------------  behind the scope

unsafeApply n a b = unsafeApply' (unsafeCoerce n) a b            
      where unsafeApply' :: Church -> (Int -> Int) -> Int -> Int  
            unsafeApply' n a b = n a b  
          