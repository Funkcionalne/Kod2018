data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read)

-- 1 bod
-- minimimum v binárnom vyhľadávacom strome
-- ideme stále doľava, a keď sa nedá, tak máme minimimum
-- usporiadanie Ord a je v definicii zbytocne, len pre pripad, ze by niekto chcel porovnavat vsetky prvky...
minim :: Ord a => Tree a -> a
minim (Node v Empty _)   = v
minim (Node _ lf _)         = minim lf
minim Empty                 = error "minim: tree is empty"

-- 3 body
-- vyhoď prvok z binárneho vyhľadávacieho stromu  
delete :: Ord a => a -> Tree a -> Tree a 
delete v Empty = Empty
delete v (Node x left Empty) | v == x = left
delete v (Node x Empty right) | v == x = right
delete v (Node x left right)
  | v == x = let k = minim right in Node k left (delete k right)
  | v < x  = Node x (delete v left) right
  | v > x  = Node x left (delete v right)
  