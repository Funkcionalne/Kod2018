module MinMax where

data BTree t = Node (BTree t) t (BTree t) | Nil deriving(Show, Eq)

minmax :: (Ord t) => (BTree t) -> (t, t)

minmax (Node Nil v Nil)    = (v, v)
minmax (Node Nil v right) = ( min v rmin, max v rmax)
	where 
	(rmin, rmax) = minmax right	
minmax (Node left v Nil) = ( min v lmin, max v lmax)
	where 
	(lmin, lmax) = minmax left
minmax (Node left v right) = ( min (min lmin rmin) v, max (max lmax rmax) v)
	where 
	(lmin, lmax) = minmax left
	(rmin, rmax) = minmax right
  
