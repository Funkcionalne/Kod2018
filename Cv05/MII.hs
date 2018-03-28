module Cvicenie5 where

import Data.List
import Data.Char
import Terms
{--
-- identifikator premennej je String 
type Var = String

-- lambda termy
data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp  deriving(Eq)
--}

-- dodefinujte show pre LExp
instance Show LExp where
  show (LAMBDA x exp) = "\\" ++ x ++ "." ++ (show exp)
  show (ID var) = var
  show (APP exp1 exp2) = "(" ++ (show exp1) ++ " " ++ (show exp2) ++ ")"
  
-- pozrite si stromovy fold na LExp  
--foldLambda :: (String -> LExp -> LExp) -> (String -> LExp) -> (LExp -> LExp -> LExp) -> LExp -> LExp
foldLambda :: (String -> t -> t) -> (String -> t) -> (t -> t -> t) -> LExp -> t
foldLambda lambda var apl (LAMBDA str exp)  = lambda str (foldLambda lambda var apl exp)
foldLambda lambda var apl (ID str)          = var str
foldLambda lambda var apl (APP exp1 exp2)   = apl (foldLambda lambda var apl exp1) 
                                                  (foldLambda lambda var apl exp2)

-- zoznam vsetkych premennych v lambda vyraze, definujte pouzitim foldLambda                                                 -
vars  :: LExp -> [String]
vars = undefined

-- definujte show pomocou foldLambda
show' :: LExp -> String
show' = undefined
                             
-- zopar pouzitelnych konstant                             
omega = (LAMBDA "x" (APP (ID "x") (ID "x")))
izero = (LAMBDA "f" (LAMBDA "x" (ID "x")))
ione =    (APP isucc izero)
itwo =    (APP isucc (APP isucc izero))
ifour =   (APP isucc (APP isucc (APP isucc (APP isucc izero))))
ieight =  (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc izero))))))))
isucc = (LAMBDA "n" 
          (LAMBDA "f" 
            (LAMBDA "x" (APP (ID "f") (APP (APP (ID "n") (ID "f")) (ID "x"))) )))
-- iplus =  fromString "?m.?n.?f.?x.((m f) ((n f) x))" 
-- itimes = fromString "?m.?n.?f.?x.((m (n f)) x)"
-- ipower = fromString "?m.?n.(n m)"  
--ithree =  (APP (APP iplus itwo) ione)
--inine =   (APP (APP itimes ithree) ithree)
--isixteen = (APP (APP ipower itwo) ifour)

-- definujte mini-syntakticky analyzator pre lambda vyrazy
-- "x"     -> ID "x"
-- "(x y)" -> (APP (ID "x") (ID "x"))
-- "\x.x"  -> (LAMBDA "x" (ID "x"))
fromString  :: String -> (LExp, String)
fromString (x:xs) | (isAlpha x) = ((ID [x]), xs)
                  | (x == '\\') = let (e,rest) = fromString (tail (tail xs)) in ((LAMBDA [head xs] e), rest)                                                 
                  | (x == '(')  = let (e1,rest1) = fromString xs in 
                                     let (e2, rest2) = fromString (tail rest1) in ((APP e1 e2), tail rest2)
                  | otherwise = error ("syntax error: " ++ (x:xs))
fromString _ = error "empty string"

-- najst vsetky podtermy termu
-- priamociaro
podtermy :: LExp -> [LExp]
podtermy = undefined

{--
podtermy (LAMBDA "x" (APP (ID "x") (ID "x")))
[\x->(x x),(x x),x]
--}

-- akumulatorom
podtermy1 :: LExp -> [LExp]
podtermy1 = undefined

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

----

-- nahradi premennu za premennu
nahrad ::  LExp -> String -> String -> LExp
nahrad (LAMBDA v1 e1) from to | (v1 /= from) = let w = (v1 ++ v1) in (LAMBDA w (nahrad (nahrad e1 v1 w) from to)) 
                              | otherwise = (LAMBDA v1 (nahrad e1 from to))
nahrad (ID v1) from to = if (v1 == from) then (ID to) else (ID v1)
nahrad (APP e1 e2) from to = (APP (nahrad e1 from to) (nahrad e2 from to))

{-
nahrad (LAMBDA "x" (APP (ID "x") (ID "x"))) "x" "y"
\x->(x x)
nahrad (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "z"
\x->(x z)
nahrad (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "x"
\x->(x x)
:-(
-}

-- test, ci premenna je volna v terme
free    :: String -> LExp -> Bool
free  = undefined

-- nahradi premennu za premennu, druhy pokus
nahrad' ::  LExp -> String -> String -> LExp
nahrad' = undefined

{-
"?: " nahrad' (LAMBDA "x" (APP (ID "x") (ID "x"))) "x" "y"
\x->(x x)
"?: " nahrad' (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "z"
\xx->(xx z)
"?: " nahrad' (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "x"
\xx->(xx x)
:-)
-}

alpha :: LExp -> LExp -> Bool
alpha  (ID x) (ID y)                = (x==y)
alpha  (LAMBDA x e1) (LAMBDA y e2)  =  alpha e1 (nahrad' e2 y x)
alpha  (APP e1 f1) (APP e2 f2)      =  (alpha f1 f2)  && (alpha e1 e2)
alpha   _ _ =  False

e1 = [
  alpha (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "x" (APP (ID "x") (ID "y")))
  ,
  alpha (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "y" (APP (ID "y") (ID "y")))
  ,
  alpha (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "y" (APP (ID "z") (ID "y")))
  ]


alphaEq :: LExp -> LExp -> Bool
alphaEq e1 e2  = alphaEq' e1 e2 (Just []) /= Nothing

alphaEq' :: LExp -> LExp -> Maybe [(Var,Var)]-> Maybe [(Var,Var)]
alphaEq'  _ _ Nothing = Nothing
alphaEq'  (ID x) (ID y) (Just pairs) = if elem (x,y) pairs then (Just pairs) else Nothing
alphaEq'  (LAMBDA x e1) (LAMBDA y e2) (Just pairs) =  alphaEq' e1 e2 (Just ((x,y):pairs))
alphaEq'  (APP e1 f1) (APP e2 f2) pairs =  alphaEq' f1 f2 (alphaEq' e1 e2 pairs)
alphaEq'  _ _ _ =  Nothing

e = [
  alphaEq (ID "x") (ID "x"),
  alphaEq (ID "x") (ID "y"), 
  alphaEq (APP (ID "x") (ID "y")) (APP (ID "x") (ID "z")),
  alphaEq (APP (ID "u") (ID "y")) (APP (ID "x") (ID "z")),
  alphaEq (APP (ID "x") (ID "x")) (APP (ID "x") (ID "y")),
  alphaEq (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "x" (APP (ID "x") (ID "y")))
  ]
  
-- alphaEq' (ID "x") (ID "x") (Just [])  

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
