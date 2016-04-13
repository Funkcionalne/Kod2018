module BinStrom (BinStrom, prazdnyStrom, jeVStrome, pridajDoStromu, 
                 zmazZoStromu, vytvorStrom, inorder) where
                 
data BinStrom a = PrazdnyBS | VrcholBS a (BinStrom a) (BinStrom a)
  deriving (Show, Read)
  
prazdnyStrom = PrazdnyBS

jeVStrome      :: (Ord a, Show a, Read a) => a -> BinStrom a -> Bool
pridajDoStromu :: (Ord a, Show a, Read a) => a -> BinStrom a -> BinStrom a
zmazZoStromu   :: (Ord a, Show a, Read a) => a -> BinStrom a -> BinStrom a
vytvorStrom    :: (Ord a, Show a, Read a) => [a] -> BinStrom a
inorder        :: (Ord a, Show a, Read a) => BinStrom a -> [a]

jeVStrome h PrazdnyBS = False
jeVStrome h (VrcholBS h1 lp pp) 
  | h == h1 = True
  | h < h1 = jeVStrome h lp
  | otherwise = jeVStrome h pp
  
pridajDoStromu h PrazdnyBS = VrcholBS h PrazdnyBS PrazdnyBS
pridajDoStromu h v@(VrcholBS h1 lp pp)
  | h == h1   = v
  | h < h1    = VrcholBS h1 ( pridajDoStromu h lp) pp
  | otherwise = VrcholBS h1 lp (pridajDoStromu h pp)

vytvorStrom vs = foldr pridajDoStromu PrazdnyBS vs

-- trocha lepsie, ale len ked je zoznam vs usporiadany neklesajuco
vytvorStrom' []  = PrazdnyBS
vytvorStrom' vs = VrcholBS v (vytvorStrom' lvs) (vytvorStrom' pvs)
  where
    lvs = take n vs
    (v:pvs) = drop n vs
    n = (length vs) `div` 2

inorder PrazdnyBS = []
inorder (VrcholBS h lp pp) = inorder lp ++ [h] ++ inorder pp
-- skusit efektivnejsie

-- zmazZoStromu je najzaujimavejsi :-)
zmazZoStromu x s = undefined

  
  