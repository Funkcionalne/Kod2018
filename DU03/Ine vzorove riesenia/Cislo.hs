--zadana funkcia
--revezuje bity
g :: Integer -> Integer -> Integer
g 0 m = m
g n m 
  | odd n  = g (n `div` 2) (2 * m + 1)
  | even n = g (n `div` 2) (2 * m)

--riesenie  
f :: Integer -> Integer
f n = g n 0

-- uprav pridava na koniec 0 (v dvojkovej sustave)
cislo :: Integer -> Integer -> Integer
cislo n x = if even x then 0 else uprav (g x 0)
  where 
    uprav x = if x > n then x else uprav ( 2*x )