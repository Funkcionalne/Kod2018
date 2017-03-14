module Derivivac where

--derivovac, autor, Miso Winczer, 2016
-----------
data Exp = Const Int
          | Var String
          | Add Exp Exp
          | Sub Exp Exp
          | UMin Exp  
          | Mul Exp Exp
          | Div Exp Exp
          | Pwr Exp Exp
          | Ln Exp
      deriving (Eq)

instance Show Exp where
      show (Const x) = show x
      show (Var x) = x
      show (Add e1 e2) = show e1 ++ " + " ++ show e2 
      show (Sub e1 e2) = show e1 ++ " - (" ++ show e2 ++ ")"
      show (UMin e) = "-(" ++ show e ++ ")"
     
-- derivivanie vyrazu podla premennej      
derive  :: Exp -> String -> Exp
derive    (Const _) _ = Const 0
derive    (Var x) y     = if  x == y then Const 1 else Const 0
derive    (Add e1 e2) x = makeAdd (derive e1 x) (derive e2 x)
derive    (Sub e1 e2) x = makeSub (derive e1 x) (derive e2 x)
derive    (UMin e) x    = UMin (derive e x)

-- jemne zjedodusujuci konstruktor suctu
makeAdd :: Exp -> Exp -> Exp
makeAdd (Const x) (Const y) = Const (x+y)
makeAdd (Const 0) e2  = e2
makeAdd e1 (Const 0)  = e1
makeAdd e1 e2         = Add e1 e2

-- jemne zjedodusujuci konstruktor suctu
makeSub :: Exp -> Exp -> Exp
makeSub (Const x) (Const y) = Const (x-y)
makeSub (Const 0) e2  = UMin e2
makeSub e1 (Const 0)  = e1
makeSub e1 e2         = Sub e1 e2

eval  :: Exp -> (String -> Exp) -> Exp
eval  (Const c) subst = Const c
eval  (Var v) subst = subst v
eval  (Add e1 e2) subst = Add (eval e1 subst) (eval e2 subst)
eval  (Sub e1 e2) subst = Sub (eval e1 subst) (eval e2 subst)

ww = eval e4 (\x -> case x of 
              "x" -> Const 1
              "y" -> Var "y"
              "z" -> (Add (Var "y") (Var "x"))
              )
              
-- dva vyrazy su komutativne ekvivalentne              
kEq   :: Exp -> Exp -> Bool
kEq  (Const c1) (Const c2 )       = c1 == c2
kEq  (Var v1) (Var v2 )           = v1 == v2
kEq  (Add e1 e2) (Add f1 f2)        = 
                  (kEq e1 f1) && (kEq e2 f2)
                  ||
                  (kEq e1 f2) && (kEq e2 f1)
kEq  (Sub e1 e2) (Sub f1 f2)        = 
                  (kEq e1 f1) && (kEq e2 f2)
                  ||
                  (kEq e1 f2) && (kEq e2 f1)

kEq  _ _ = False


----------------- priklady
c1 = Const 1
cm1 = Const (-1)
c2 = Const 2
c3 = Const 3
vx = Var "x"
vy = Var "y"
e3 = Add c1 c2  -- 2+3
e4 = Add c3 vx  -- 3+x 
e5 = Add c2 vy  -- 2+y
e6 = Add e3 e4  -- (2+3)+(3+x)
e7 = Add e4 e3  -- (3+x)+(2+3)
e8 = Add e4 e4  -- (3+x)+(3+x)
e9 = Sub e3 e6  -- 2+3 - ((2+3)+(3+x))
e10 = Sub e9 e9 -- 0


-- dopiste derivovanie Mul, Div, Pwr, Ln

-- najdite vyraz, ktory po zderivivani neupravi
{--
"?: " derive (Add (Add (Var "x") (Const 1)) (Add (Var "x") (Var "x"))) "x"
3
--}
-- definujte 
simplify :: Exp -> Exp
simplify  (Const c) = Const c
simplify  (Var v) = Var v
simplify  (Add e1 e2) = makeAdd e1 e2
        where ee1 = simplify e1 
              ee2 = simplify e2
simplify  (Sub e1 e2) = makeSub e1 e2
        where ee1 = simplify e1 
              ee2 = simplify e2

simplifyFix    :: Exp -> Exp
simplifyFix   e   = if e == ee then e else simplifyFix ee
        where ee = simplify e


        