module Halda (Halda, prazdnaHalda, jePrazdnaHalda, vrcholHaldy, 
              vlozDoHaldy, zoberZHaldy, vytvorHaldu) where
-- podla Rabhi, Lapalme: Algorithms, A Functional Programming Approach
-- Addison-Wesley 1999
              
data Halda a = PrazdnaH | VrcholH a Int (Halda a) (Halda a)
  deriving (Show, Read)

prazdnaHalda :: (Ord a) => Halda a  
jePrazdnaHalda :: (Ord a) => Halda a -> Bool
vrcholHaldy :: (Ord a) => Halda a -> a
vlozDoHaldy :: (Ord a) => a -> Halda a -> Halda a
zoberZHaldy :: (Ord a) => Halda a -> Halda a
vytvorHaldu :: (Ord a) => [a] -> Halda a
  
prazdnaHalda = PrazdnaH

jePrazdnaHalda PrazdnaH = True
jePrazdnaHalda _ = False

vrcholHaldy PrazdnaH = error "vrcholHaldy: prazdna halda"
vrcholHaldy (VrcholH h _ _ _) = h

rank :: (Ord a) => Halda a -> Int
rank PrazdnaH = 0
rank (VrcholH _ r _ _) = r

vytvorVrcholH :: (Ord a) => a -> Halda a -> Halda a -> Halda a
vytvorVrcholH x lh ph 
  | rank lh >= rank ph = VrcholH x (rank ph + 1) lh ph
  | otherwise          = VrcholH x (rank lh + 1) ph lh

zlucH :: (Ord a) => Halda a -> Halda a -> Halda a
zlucH h PrazdnaH = h
zlucH PrazdnaH h = h
zlucH h1@(VrcholH x _ lh1 ph1) h2@(VrcholH y _ lh2 ph2)
  | x <= y = vytvorVrcholH x lh1 (zlucH ph1 h2)
  | otherwise = vytvorVrcholH y lh2 (zlucH h1 ph2)

  
vlozDoHaldy x h = zlucH (VrcholH x 1 PrazdnaH PrazdnaH) h

zoberZHaldy PrazdnaH = error "zoberZHaldy: prazdna halda"
zoberZHaldy (VrcholH _ _ lh ph) = zlucH lh ph

vytvorHaldu = foldr vlozDoHaldy PrazdnaH 
  
  