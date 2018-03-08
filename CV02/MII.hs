module Cvicenie2 where

import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)


slova   :: Int -> [Char] ->[[Char]]
slova = error "tbd"


bajty :: Int -> [String]
--bajty 1 = ["0","1"]
bajty 0 = [""]
bajty n = [ y:x | x <- xs, y <- ['0','1']]
          where xs = bajty(n-1)

qchb = quickCheck( \n -> (n >= 0 && n < 16) ==> length (bajty n) == 2^n )
qchb2 = quickCheck( \n -> (n >= 0 && n < 16) ==> (filter (/=n) (map length (bajty n))) == [] ) 

 
powerSet  :: [Int] -> [[Int]]
powerSet [] = [[]]
powerSet (x:xs) = [x:ys | ys <- tmp] ++tmp
          where tmp = powerSet xs
-- definujte a overte nejaku vlastnost funkcie powerSet    
qchps = quickCheck( \xs -> (length xs < 20) ==> length (powerSet xs ) == 2^(length xs) )
-- naprogramuje jeden prechod bubble sort algoritmu
bubble :: [Int] -> [Int]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs) = if x>y then y:(bubble(x:xs))
                  else x:(bubble(y:xs))

qchbs = quickCheck( \xs -> (last(bubble xs)) == maximum(bubble xs))
                  
-- definujte a overte nejaku vlastnost funkcie bubble                    
-- qch1 = quickCheck( \xs -> cond ==> proposition )
-- qch1 = verboseCheck( \xs -> cond ==> proposition )

-- definujte predikat pre usporiadany/rastuci/nerastuci zoznam
ordered :: [Int] -> Bool
ordered [] = True
ordered [_] = True
ordered (x:xs@(y:ys)) = x<=y && ordered xs

-- definujte a overte nejaku vlastnost funkcie ordered

-- qch2 = quickCheck( \xs -> cond ==> proposition )
-- qch2 = verboseCheck( \xs -> cond ==> proposition )

-- pouzite bubble na bubbleSort
bubbleSort  :: [Int]->[Int]
bubbleSort xs = (iterate bubble xs)!!(length xs)
e = bubbleSort [4,3,4,6,7,4,3,1,1,2,3,4,5,6,7,8,9,0,5,3,2,3,2,3,4,5,6,7,1,2,2,0,9,12,11]
--[0,0,1,1,1,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,5,5,5,6,6,6,7,7,7,8,9,9,11,12]

-- definujte a overte nejaku vlastnost funkcie bubbleSort
-- qch3 = quickCheck( \xs -> cond ==> proposition )
-- qch3 = verboseCheck( \xs -> cond ==> proposition )

insert  :: Int -> [Int]->[Int]
insert = error "tbd"

qch3 = quickCheck(\x -> \xs -> ordered xs ==> ordered(insert x xs))

insertSort  :: [Int]->[Int]
insertSort = error "tbd"

qch4 = quickCheck(\xs -> ordered(insertSort xs))
     
merge :: (Ord t) => [t] -> [t] -> [t]
merge = error "tbd"
                
qch5 = verboseCheck(\xs -> \ys -> 
        length xs < 6 && length ys < 6 &&
        ordered xs  && ordered ys
        ==> ordered(merge xs ys))
                
----------------------------------------------------------------------
------------------------------------------------------------------
-- kompozícia zoznamu funkcií,  :: [a->a] -> (a->a)

-- zaciatocnicka definicia cez zoznamovu rekurziu
kompozicia  :: [a->a] -> (a->a)
kompozicia = error "tbd"

-- jemne pokrocily haskellista, ktory bol na prednaske
kompozicia'  :: [a->a] -> (a->a)
kompozicia' = error "tbd"

-- definicia haskellistu, co si nasiel operator $
kompozicia''  :: [a->a] -> (a->a)
kompozicia'' = error "tbd"

-- haskellista, co si pamata, ze skladanie funkcii je asociativne ale nepamata, ze nie je komutativne
kompozicia''''  :: [a->a] -> (a->a)
kompozicia'''' = error "tbd"

-- definicia haskellistu, co si este prehodil x na lavu stranu
kompozicia'''''  :: [a->a] -> (a->a)
kompozicia''''' = error "tbd"

-- haskellista, co bude volit lavicu
kompoziciaLeft  :: [a->a] -> (a->a)
kompoziciaLeft = error "tbd"

-- haskellista, co bude volit neexistujucu pravicu
kompoziciaRight  :: [a->a] -> (a->a)
kompoziciaRight = error "tbd"

zoznamfcii = []                

