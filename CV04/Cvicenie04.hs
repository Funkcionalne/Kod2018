module Cvicenie04 where

--derivovac, autor, Miso Winczer, 2016
-----------
data Exp = Const Int
          | Var String
          | Add Exp Exp
          | Sub Exp Exp
          -- | UMin Exp  
          | Mul Exp Exp
          | Div Exp Exp
          -- | Pwr Exp Exp
          -- | Ln Exp
      deriving (Eq, Read)

instance Show Exp where
    show (Const x) = show x
    show (Var x) = x
    show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Mul x y) = show x ++ " * " ++ show y
    show (Div x y) = show x ++ " / " ++ show y

e1	:: Exp	
e1 = Div 
        (Mul (Add (Var "x")(Var "x") ) (Sub (Var "x")(Const 1))	)
        (Var "x")
e2	:: Exp	
e2 = (Mul (Add (Var "x") (Var "x")) (Sub (Var "x") (Const 1) )) 

type Substitucia = String -> Exp
		
eval :: Exp -> Substitucia -> Exp
eval (Const x) _ = Const x
eval (Var x) s = s x
eval (Add x y) s = Add (eval x s) (eval y s)
eval (Sub x y) s = Sub (eval x s) (eval y s)
eval (Mul x y) s = Mul (eval x s) (eval y s)
eval (Div x y) s = Div (eval x s) (eval y s)

s :: Substitucia
s = (\var -> case var of
        "x" -> Const 2
        "y" -> Const 6)
		
derive :: Exp -> String -> Exp
derive (Const x) var = (Const 0)
derive (Var x) var | x == var = (Const 1)
                   | otherwise = (Const 0)
derive (Add e1 e2) var = (Add (derive e1 var) (derive e2 var))
derive (Sub e1 e2) var = (Sub (derive e1 var) (derive e2 var))
derive (Mul e1 e2) var = (Add   e' e'' )
                         where e' = Mul (derive e1 var) e2
                               e'' = Mul (derive e2 var) e1
derive (Div e1 e2) var = (Div citatel menovatel)
                         where e' = Mul (derive e1 var) e2
                               e'' = Mul (derive e2 var) e1
                               citatel = (Sub e' e'')
                               menovatel = (Mul e2 e2)	

simply :: Exp -> Exp
simply (Add (Const x) (Const y)) = Const (x+y)
simply (Sub (Const x) (Const y)) = Const (x-y)	
simply (Mul (Const x) (Const y)) = Const (x*y)	
simply (Div (Const x) (Const y)) = Const (x `div` y)	
simply (Add (Const 0) e) = e
simply (Add e (Const 0)) = e
simply (Add e1 e2) | e1 == e2 = Mul (Const 2) e1
				   | otherwise = Add (simply e1) (simply e2)
simply (Mul (Const 0) _) = Const 0
simply (Mul _ (Const 0)) = Const 0
simply (Mul (Const 1) e) = e
simply (Mul e (Const 1)) = e
simply (Div e (Const 1)) = e
simply (Mul (Const x) (Add e1 e2)) = Add (Mul (Const x) e1) (Mul (Const x) e2) 
simply (Mul (Add e1 e2) (Const x)) = Add (Mul (Const x) e1) (Mul (Const x) e2) 
simply (Mul (Const x) (Sub e1 e2)) = Sub (Mul (Const x) e1) (Mul (Const x) e2) 
simply (Mul (Sub e1 e2) (Const x)) = Sub (Mul (Const x) e1) (Mul (Const x) e2) 
simply (Mul e1 e2) = Mul e1' e2'
        where 
			e1' = simply e1
			e2' = simply e2
simply (Sub e1 e2) = Sub e1' e2'
        where 
			e1' = simply e1
			e2' = simply e2
simply (Const x) = Const x
simply (Var x) = Var x

fix :: (Exp -> Exp) -> Exp -> Exp
fix f e = if e /= e' then fix f e' else e 
			where e' = f e
