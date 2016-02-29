module Hanoi where

-- riesenie pre tri veze
hanoi :: Integer -> String -> String -> String -> [(String, String)]
hanoi 0 from to aux = []
hanoi n from to aux = hanoi (n-1) from aux to ++ [(from,to)] ++ hanoi (n-1) aux to from

-- riesenie pre vseobecny pocet vezi
hanois :: Integer -> [String] -> [(String, String)]
-- hanois = undefined  
-- riesenie studenta
hanois 0 _  = []
hanois 1 [from, to] 		= [(from, to)]
hanois 2 [from, to] 		= undefined
hanois n [from, to, aux] 	= hanoi n from to aux
hanois n (from:to:aux:rest) = 		hanois k (from:aux:to:rest) ++ 
									hanois (n-k) (from:to:rest) ++ 
									hanois k (aux:to:from:rest)
        where k = n `div` 2
-- length $ hanois 15 ["a", "b", "c", "d" ]  
