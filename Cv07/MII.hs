{-# LANGUAGE RankNTypes #-}
module Cvicenie7 where
import Unsafe.Coerce 

type Var = String
data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp deriving (Show)

-- rovnake :: LExp -> LExp -> Bool, ktora porovna dva lambda termy,ci su rovnake.

rovnake :: LExp -> LExp -> Bool
rovnake (LAMBDA v1 e1) (LAMBDA v2 e2) = (v1 == v2) && rovnake e1 e2
rovnake (ID v1) (ID v2) = v1 == v2
rovnake (APP e11 e21) (APP e12 e22) = (rovnake e11 e12) && (rovnake e12 e22)
rovnake _ _ = False
---------------------------------------------------------------

-- nahradi premennu za inu premennu 
nahrad :: LExp -> String -> String -> LExp
nahrad (ID x) from to = if x==from then ID to else ID x
nahrad (LAMBDA x exp) from to = LAMBDA x' exp' 
                                    where 
                                        x' = if x==from then to else x 
                                        exp' = nahrad exp from to
                                        
nahrad (APP exp1 exp2) from to = (APP (nahrad exp1 from to) (nahrad exp2 from to))

-- nahradi premennu za inu premennu - vylepsene
nahrad2 :: LExp -> String -> String -> LExp
nahrad2 (ID x) from to = if x==from then ID to else ID x
nahrad2 (LAMBDA x exp) from to 
                        | from==x = LAMBDA x exp 
                        | (x==to) && isFree from exp = let x'= x++x in (LAMBDA x' (nahrad2 (nahrad2 exp x x') from to))
                        | otherwise = LAMBDA x (nahrad2 exp from to)
                                        
nahrad2 (APP exp1 exp2) from to = (APP (nahrad2 exp1 from to) (nahrad2 exp2 from to))


{--  --}
test1 = [
  nahrad2 (LAMBDA "y" (ID "x")) "y" "x",
  nahrad2 (LAMBDA "x" (APP (ID "x") (ID "x"))) "x" "y",  -- \x->(x x)
  nahrad2 (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "z",  -- \xx->(xx z)
  nahrad2 (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "x"   -- \xx->(xx x) ???
  ]

-- `free l` vráti zoznam všetkých premenných, ktoré majú voľný výskyt v terme `l`
free  :: LExp -> [Var]
free  (ID x)       = [x]
free  (LAMBDA x e) = filter (/=x) (free e)
free  (APP e1 e2)  = (free e1) ++ (free e2)

-- test, ci premenna je volna v terme
isFree      :: String -> LExp -> Bool
isFree v e  = elem v $ free e

alpha   :: LExp -> LExp -> Bool
alpha (ID x) (ID y) = x==y
alpha (APP exp1 exp2) (APP exp1' exp2') = (alpha exp1 exp1') && (alpha exp2 exp2')
alpha (LAMBDA x expx) (LAMBDA y expy)
                                | x==y = alpha expx expy
                                | x/=y = alpha expx (nahrad2 expy y x)
alpha _ _ = False

test2 = [
  alpha (LAMBDA "x" (ID "a")) (LAMBDA "y" (ID "b")),
  alpha (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "x" (APP (ID "x") (ID "y"))),
  alpha (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "y" (APP (ID "y") (ID "y"))),
  alpha (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "y" (APP (ID "z") (ID "y")))
  ]

---------------------------------------------------------------
  
alphaEq :: LExp -> LExp -> Bool
alphaEq e1 e2  = alphaEq' e1 e2 (Just []) /= Nothing

alphaEq' :: LExp -> LExp -> Maybe [(Var,Var)]-> Maybe [(Var,Var)]
alphaEq'  = undefined

test3 = [
  alphaEq (ID "x") (ID "x"),
  alphaEq (ID "x") (ID "y"), 
  alphaEq (APP (ID "x") (ID "y")) (APP (ID "x") (ID "z")),
  alphaEq (APP (ID "u") (ID "y")) (APP (ID "x") (ID "z")),
  alphaEq (APP (ID "x") (ID "x")) (APP (ID "x") (ID "y")),
  alphaEq (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "x" (APP (ID "x") (ID "y")))
  ]
--------------------------------------------------------------  

-- zdroj http://techtipshoge.blogspot.sk/2011/06/church-number-with-ski-combinators.html
-- http://www.angelfire.com/tx4/cus/combinator/birds.html

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
-- one  
one = \f -> \x -> f x  
one''   = i  
one'  = (s ( (s (k s)) ((s (k k) i)) )) (k i)
two   = \f -> \x -> f (f x)
two'   = (s (s (k s) k)) i  
three = (s (s (k s) k)) (s (s (k s) k) i)  
four  = (s (s (k s) k)) ((s (s (k s) k)) (s (s (k s) k) i))  

-- For assertion only  
type Church = (Int -> Int) -> Int -> Int  
  
-- integer -> church  
church n = \f -> \x -> iterate f x !! n  

-- n++  (succ z prednasky)
incr = \n -> \f -> \x -> f (n f x)     
incr' n = \f -> \x -> f (n f x)     

add = \m -> \n -> \f -> \x -> m f (n f x)     
add' m n = \f -> \x -> m f (n f x)     
  
-- m*n  
mult = \m -> \n -> \f -> \x -> m (n f) x  
  
-- m^n  
expt = \m -> \n -> n (mult m) one  
   
true  x y = x
false x y = y

-- AND 		:= λx.λy. x y FALSE := λxy.x y FALSE
-- OR 		:= λx.λy. x TRUE y := λxy.x TRUE y
ch_and x y   = x y false
ch_or x y    = x true y
ch_not x     = x false true
ch_xor       = undefined

--PAIR	:= λx.λy.(λc. c x y) := λxyc. c x y
-- LEFT 	:= λx.x TRUE
-- RIGHT 	:= λx.x FALSE

ch_pair x y  = \c -> c x y
ch_left = \x -> x true
ch_right = \x -> x false

{-
Nil 	= λz.z TRUE FALSE FALSE
Cons	= λx.λy.λz.z FALSE x y

head	= λp.p (λx.λy.λz.y)
tail		= λp.p (λx.λy.λz.z)
isNil	= λp.p (λx.λy.λz.x)
-}
nil       = \z -> z true false false
cons x y  =  \z -> z false x y
ch_head p  = p (\x y z -> y)
ch_tail p  = p (\x y z -> z)
ch_isNil p  = p (\x y z -> x)

--omega = \x -> (x x)
-- bigomega = omega omega
-- ypsilon = \f x -> (f (x x) f (x x))

{-
len :: (forall t. ((t2 -> t1 -> t2) -> (t3 -> t4 -> t4) -> (t5 -> t6 -> t6) -> t)
     -> t) -> (t->t) -> t -> t        
len lst  =
     ifte (ch_isNil lst)
          zero
          (incr (len (ch_tail lst)))
-}  
  
-- isZero
isZero n = n (\_ -> false) true

decr n =
    n (\m f x-> f (m incr zero))
    zero
    (\x->x)
    zero
    
ifte  c t e = c t e
    
fact :: (forall a. (a->a)->a->a) -> (a->a) -> a -> a        
fact n  =
     ifte (isZero n)
          one
          (mult n (fact (decr n)))
    
unChurch n = n (+1) (0)  
            
a = church 2  
b = church 10   
  
main = do print $ unChurch $ add a b  
          print $ unChurch $ incr a  
          print $ (isZero $  incr a) "T" "F"  -- false
          print $ (isZero zero) "T" "F"  -- true
          print $ unChurch $ mult a b  
          print $ unChurch $ expt a b 
          print $ unChurch $ i a
          print $ unChurch $ k a b
          print $ unChurch zero  
          print $ unChurch one  
          print $ unChurch one'
          print $ unChurch two  
          print $ unChurch three  
          print $ unChurch four  
          print $ (ch_and true true)"T" "F"
          print $ (ch_and true false)"T" "F"
          print $ (ch_and false false) "T" "F"
          print $ unChurch (ch_left $ ch_pair one two) 
          print $ unChurch $ ch_head (cons one nil)          
          print $ unChurch $ fact b
          print $ unsafeApply apply' (+1) 10   
          print $ unsafeApply apply'' (^2) 10   
          print $ unsafeApply apply''' (subtract 99) 10 
          
          
unsafeApply n a b = unsafeApply' (unsafeCoerce n) a b            
      where unsafeApply' :: Church -> (Int -> Int) -> Int -> Int  
            unsafeApply' n a b = n a b  

 