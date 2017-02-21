module H3 where
import Data.Char
import Test.QuickCheck

-- typy Int, Integer, Float, Char, String

fib ::  Integer -> Integer
fib 0   = 1
fib 1   = 1
fib n   = fib (n-1) + fib (n-2)

fib' ::  Integer -> Integer
fib' n  | n < 2     = 1
        | otherwise = fib (n-1) + fib (n-2)

{-- 
comment 
--}        
-- 5 = 101, 10 = 1010        
binnib  :: Int -> Bool
binnib n = (binrev n 0 == n)
      where
        binrev :: Int -> Int -> Int
        binrev 0 acc = acc
        binrev n acc =  binrev (n `div` 2)  (2*acc + n `mod` 2)
        
-- { i | i in {1..n}, binnib i}        
allSym    :: Int -> [Int]
allSym n  = [ i | i <- [1..n], binnib i]

-- type String = [Char]
--  int2String 5 = "101"
--  int2String 6 = "110"
int2String    :: Int -> String
int2String 0  = ""    -- []
int2String 1  = "1"    -- []
int2String n  =  int2String (n `div` 2) ++ [chr (n `mod` 2 + (ord '0'))]

-- string2Int "110" = ['1','1','0'], x = '1', xs = "10"
-- 011
string2Int  :: [Char] -> Int
string2Int str = string2Int' (reverse str)
        where 
          string2Int'  :: [Char] -> Int
          string2Int'  []   = 0
          string2Int'  (x:xs)   = 2*(string2Int' xs) + (ord x - ord '0')
        
binnib'  :: Int -> Bool
binnib' n = (string2Int (reverse (int2String n))) == n

allSym' n  = [ i | i <- [1..n], binnib' i]

qch1 = quickCheck((\n -> (binnib n == binnib' n)))

qch2 = quickCheck((\n -> (n>0) ==> (binnib n == binnib' n)))

type Mnozina t = [t]
powerSet  :: Mnozina t -> [Mnozina t]
-- powerSet  :: [t] -> [[t]]
powerSet  []  = [[]]
powerSet  (x:xs)  =  let pom = powerSet xs in [ x:m | m <- pom] ++ pom

qch3 = quickCheck((\xs -> (length xs < 13) ==> length (powerSet xs) == 2^(length xs))::[Int]->Property)


