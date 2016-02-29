module Del11 where

-- test deletelnosti jedenastimi
-- cislo je delitelne jedenastimi prave vtedy, ak rozdiel jeho cifier na parnych a neparnych
-- poziciach je delitelne jedenastimi, okrem toho 0 je delitelne jedenastimi a 
-- ziadne ine jednociferne cislo nie je. riesenie tvaru n `mod` 11 == 0 nie je spravne

delitelne11	:: Integer -> Bool
delitelne11 0	= True
delitelne11 n	| n < 10	= False
				| otherwise = delitelne11 (del11 n 0 True)
				
del11	:: Integer -> Integer -> Bool -> Integer
del11 0 aux	_ = abs aux
del11 n aux flag = del11 (n `div` 10) (if flag then aux+c else aux-c) (not flag)
				where c = n `mod` 10
