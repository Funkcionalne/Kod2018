sito (x:xs) = x : sito(filtruj x xs)
filtruj n (x:xs) = if x `mod` n == 0 then filtruj n xs
                                     else x : filtruj n xs

cyklus = 1 : 2 : cyklus

incList (x:xs) = (x+1):incList xs

ints = 0 : incList ints

nulaZac xs = 0:xs
ineInts = s2
  where s1 = incList s2
        s2 = nulaZac s1
  
