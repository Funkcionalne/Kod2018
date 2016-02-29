module CifSucet where

import Data.Char

jCislaPocet' :: Integer -> Integer -> Integer
jCislaPocet' from to =  
  toInteger $ length $ filter (\x->((==5) $ head $ dropWhile (>9) $ iterate (toInteger . sum . map digitToInt . show)x)) [from..to]

jCislaPocet'' from to =  
   filter (\x->((==5) $ head $ dropWhile (>9) $ iterate (toInteger . sum . map digitToInt . show)x)) [from..to]
  
jCislaPocet :: Integer -> Integer -> Integer
jCislaPocet from to = (\from -> \to -> (to-from) `div` 9 +
    if (from `mod` 9 == 5) || ( to `mod` 9 == 5 ) then 1 else 0)
    from to  
    
 --   (toInteger (fromEnum (from `mod` 9 == 5)) * (toInteger(fromEnum( to `mod` 9 == 5))))) from to  



--jCislaPocet from to = (\from -> \to -> (to-from+1) `div` 9 + toInteger (fromEnum (from `mod` 9 <= 5 && to `mod` 9 >= 5))) from to  

--"?: " jCislaPocet (product [1..30]) (product[1..30]+1000)
--111
