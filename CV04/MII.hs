module Cvicenie04 where

-----------
data Exp = Const Int
          | Var String
          | Add Exp Exp
          | Sub Exp Exp
          | Mul Exp Exp
      deriving (Eq, Read)

instance Show Exp where
    show (Const x) = show x
    show (Var x) = x
    show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Mul x y) = show x ++ " * " ++ show y

e1	:: Exp	
e1 = Add 
        (Mul (Add (Var "x")(Var "x") ) (Sub (Var "x")(Const 1))	)
        (Var "x")
e2	:: Exp	
e2 = (Mul (Add (Var "x") (Var "x")) (Sub (Var "x") (Const 1) )) 

type Substitucia = String -> Exp		
s :: Substitucia
s = (\var -> case var of
        "x" -> Const 2
        "y" -> Const 6)

eval :: Exp -> Substitucia -> Exp
eval  = undefined
		
derive :: Exp -> String -> Exp
derive   = undefined

simply :: Exp -> Exp
simply   = undefined

fix :: (Exp -> Exp) -> Exp -> Exp
fix  = undefined