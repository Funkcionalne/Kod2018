module H3 where

import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)

bubble  :: [Int] -> [Int]
bubble []     = []
bubble [x]    = [x]
bubble (x:xs@(y:ys)) | x <= y = x : (bubble xs)
                | otherwise = y : (bubble (x:ys))
                
qch1 = verboseCheck(\xs -> length xs > 0 ==> last (bubble xs) == maximum xs)
qch2 = quickCheck(\xs -> length xs > 0 ==> last (bubble xs) == maximum xs)

bubbleSort  :: [Int] -> [Int]
bubbleSort xs = (iterate bubble xs)!!(length xs)

ordered :: [Int] -> Bool
ordered []  = True
ordered [x]  = True
ordered (x:xs@(y:ys)) = x <= y && ordered xs

qch3 = quickCheck(\xs -> ordered(sort xs))
qch33 = quickCheck(\xs -> ordered(4:(sort xs)))
qch333 = quickCheck(\xs -> ordered(4:sort (2:xs)) == False)


qch4 = quickCheck(\xs -> ordered(bubbleSort xs))
qch5 = verboseCheck(\xs -> ordered(bubbleSort xs))

-------------------
-- Eq t, == !=
-- Ord t, <>, max, min

merge   :: (Ord t) => [t] -> [t] -> [t]
merge   xs []   = xs
merge   [] ys   = ys
merge   a@(x:xs)  b@(y:ys)  = if x < y then x : merge xs b
                              else y : merge a ys
                              
qch6 = verboseCheck((\xs -> \ys -> 
    (length xs) + (length ys) == length (merge xs ys)) :: [Int] -> [Int] -> Bool )
    
qch7 = verboseCheck((\xs -> \ys -> 
        let sortedxs = sort xs 
            sortedys = sort ys 
            zs = merge sortedxs sortedys 
         in zs == sort zs) :: [Int] -> [Int] -> Bool)
                             
qch8 = verboseCheck((\xs -> \ys -> 
        let 
          sortedxs = sort xs 
          sortedys = sort ys
          zs = merge sortedxs sortedys in zs == sort (xs ++ ys)) 
          :: [Int] -> [Int] -> Bool)

--------------------------------------------------------
next  :: Double -> Double -> Double
next n old = (old + n/old)/2

sqrtSequence  :: Double -> [Double]
sqrtSequence n = iterate (next n) n

epsilon6::Double
epsilon6  = 1e-6

epsClose   :: [Double] -> Double -> Double
epsClose (x:y:ys) eps   = if abs(x-y) < eps then (x+y)/2
                          else epsClose (y:ys) eps

sqrt 0 = 0
sqrt n | n < 0  = 0/0 -- Double.NaN -- error "nieco zle sa stalo"
       | otherwise = epsClose (sqrtSequence n) epsilon6


qch9 = verboseCheck(\f -> abs (Prelude.sqrt f - H3.sqrt f) < epsilon6)
qch10 = quickCheck(\f -> f > 0.5 ==> abs (Prelude.sqrt f - H3.sqrt f) < epsilon6)

----------------------------------------
-- derivuj (Double->Double)-> Double -> Double -> Double
-- derivuj f bod eps
-- derivuj (\x -> x*x) 3 epsilon6 = 6
-- derivuj (\x -> x*x*x) 3 epsilon6 = 27

numerickaAproximacia :: (Double->Double) -> Double -> Double -> Double
numerickaAproximacia f x h = (f (x+h)-f x )/h

derivujSequence f x = map (numerickaAproximacia f x) (iterate (/2) 10)

derivuj f x = epsClose (derivujSequence f x) epsilon6

derivujfx f = (\x -> epsClose (derivujSequence f x) epsilon6)







