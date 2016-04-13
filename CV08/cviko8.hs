--import BinStrom 

import Halda
import BinStrom

------- Triedenie haldovanim

heapSort :: Ord a => [a] -> [a]
heapSort [] = []
heapSort xs = makeList(vytvorHaldu xs) 
 
makeList :: Ord a => Halda a -> [a]
makeList x 
  | jePrazdnaHalda x = [] 
  | otherwise = (vrcholHaldy x):makeList(zoberZHaldy x)

-- trocha inak
{-  pristupuje k VrcholH, ktory sa z Halda nevyvaza
heapSort1 :: (Ord a) => [a] -> [a]
heapSort1 zoz = zober (vytvorHaldu zoz) 
 
zober :: (Ord a) => Halda a -> [a]
zober prazdnaHalda = []
zober h@(VrcholH x i h1 h2) = x:(zober (zoberZHaldy h))   
-} 
