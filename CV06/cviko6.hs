-------

import Data.List
-- import Terms
-- module Terms where
 
-- identifikator premennej je String 
type Var = String

-- lambda termy
data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp  deriving(Eq)

instance Show LExp where
  show (LAMBDA v e) = '\\' : v ++ "->" ++ show e
  show (ID v) = v
  show (APP e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  
ione =    (APP isucc izero)
itwo =    (APP isucc (APP isucc izero))
ifour =   (APP isucc (APP isucc (APP isucc (APP isucc izero))))
ieight =  (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc izero))))))))
-- ithree =  (APP (APP iplus itwo) ione)
-- inine =   (APP (APP itimes ithree) ithree)
-- isixteen = (APP (APP ipower itwo) ifour)

izero = (LAMBDA "f" (LAMBDA "x" (ID "x")))
omega = (LAMBDA "x" (APP (ID "x") (ID "x")))
isucc = (LAMBDA "n" 
          (LAMBDA "f" 
            (LAMBDA "x" (APP (ID "f") (APP (APP (ID "n") (ID "f")) (ID "x"))) )))
-- iplus =  fromString "?m.?n.?f.?x.((m f) ((n f) x))" 
-- itimes = fromString "?m.?n.?f.?x.((m (n f)) x)"
-- ipower = fromString "?m.?n.(n m)"  

-- najst vsetky podtermy termu
-- priamociaro
podtermy :: LExp -> [LExp]
podtermy = nub . podtermy'

podtermy' :: LExp -> [LExp]
podtermy' (LAMBDA v e1) = e1 : podtermy' e1
podtermy' (APP e1 e2) = podtermy' e1 ++ podtermy' e2
podtermy' v = [v]

-- akumulatorom
podtermy1 :: LExp -> [LExp]
podtermy1 = nub . (podtermy1' [])

podtermy1' :: [LExp] -> LExp -> [LExp]
podtermy1' vys (LAMBDA v e1) = podtermy1' (e1:vys) e1
podtermy1' vys (APP e1 e2) = podtermy1' (podtermy1' vys e2) e1
podtermy1' vys v = v:vys

-- porovnat dva termy, ci su rovnake rozcvicka
-- Napíšte do modulu Rovnake funkciu rovnake
-- modul Rovnake where
-- import TypyLExp
-- -- type Var = String
-- -- data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp
-- rovnake :: LExp -> LExp -> Bool
-- rovnake = ...
-- ktora porovna dva lambda termy,ci su rovnake.
-- Príklady:
-- rovnake (ID "x") (ID "x") vráti True
-- rovnake (ID "x") (ID "y") vráti False
-- rovnake (LAMBDA "x" (ID "x")) (LAMBDA "x" (ID "x")) vrati True
-- rovnake (LAMBDA "x" (ID "x")) (LAMBDA "y" (ID "y")) vrati False
 
-- riesenie rozcvicky
rovnake :: LExp -> LExp -> Bool
rovnake (LAMBDA v1 e1) (LAMBDA v2 e2) = (v1 == v2) && rovnake e1 e2
rovnake (ID v1) (ID v2) = v1 == v2
rovnake (APP e11 e21) (APP e12 e22) = (rovnake e11 e12) && (rovnake e12 e22)
rovnake _ _ = False

vx = ID "x"
vy = ID "y" 
l1 = LAMBDA "x" vx
l2 = LAMBDA "y" vy
a1 = APP vx vy
a2 = APP vx vx

-- porovnat dva termy aj vzhladom na premenovanie premennych
rovnakeAlfa :: LExp -> LExp -> Bool
rovnakeAlfa = rovAlfa []

rovAlfa :: [(String,String)] -> LExp -> LExp -> Bool
rovAlfa zp (LAMBDA v1 e1) (LAMBDA v2 e2) = (v1 == v2) && rovnake e1 e2
-- nedokoncene
-- rovnake (ID v1) (ID v2) = v1 == v2
-- rovnake (APP e11 e21) (APP e12 e22) = (rovnake e11 e12) && (rovnake e11 e12)

-- urobit redukcie
-- (\f.\x.(f 4 x))(\y.\x.(+ x y)) 3
-- ----------redex---------------
-- (\x.((\y.\x.(+ x y)) 4 x)) 3
-- ---------redex1-------------
  -- /  ------redex2-----
 -- /                    \
-- (\y.\x.(+ x y)) 4 3   \x.((\x.(+ x 4)) x) 3
 -- ------redex------    ------redex1---------         
   -- /                    /  ----redex2---
  -- /                    /               \
-- \x.(+ x 4) 3         \x.(+ x 4) 3      \x.(+ x 4) 3
-- ------------
-- (+ 3 4)
-- -------
-- 7


-- fix moze sa zist... 
fix                   :: Eq a => (a -> a) -> a -> a
fix f x               =  if x == x' then x else fix f x'
                         where x' = f x

-- binarny vyhladavaci strom
data BinTree a = Empty | Branch a (BinTree a) (BinTree a)
   deriving(Eq, Ord, Show, Read)

makeBVS :: Ord a => [a] -> BinTree a
makeBVS [] = Empty
makeBVS (x:xs) = insertBVS x (makeBVS xs)

insertBVS :: Ord a => a -> BinTree a -> BinTree a
insertBVS x Empty = Branch x Empty Empty
insertBVS x (Branch y l r)
  | x <= y = Branch y (insertBVS x l) r
  | otherwise = Branch y l (insertBVS x r)


