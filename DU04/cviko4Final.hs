--derivovac
-----------

data Exp = ICon Int
           | Var String
           | Add Exp Exp
           | Sub Exp Exp
           | UMin Exp
           | Mul Exp Exp
           | Div Exp Exp
           | Pwr Exp Exp
           | Ln Exp
          deriving (Eq, Ord, Read, Show)
  
derive :: Exp -> String -> Exp
derive (ICon _) _ = ICon 0
derive (Var x) y | x == y    = ICon 1
                 | otherwise = ICon 0
derive (Add e1 e2) x = urobAdd (derive e1 x) (derive e2 x)
derive (Sub e1 e2) x = urobSub (derive e1 x) (derive e2 x)
derive (UMin e) x = UMin (derive e x)
derive (Mul e1 e2) x = urobAdd (urobMul (derive e1 x) e2)
                               (urobMul e1 (derive e2 x))
derive (Div e1 e2) x = urobDiv (urobAdd (urobMul (derive e1 x) e2)
                                        (urobMul e1 (derive e2 x)))
                               (urobMul e2 e2)
-- e1^{e2} = e^{e2*ln(e1)} a teraz mozeme derivovat
-- (e^{e2*ln(e1)})' = e^{e2*ln(e1)}*(e2*ln(e1))' =
--  e1^{e2}*(e2'*ln(e1)+e2*(ln(e1))') =
--  e1^{e2}*(e2'*ln(e1)+e2*(e1)'/e1)
derive (Pwr e1 e2) x =
  urobMul (urobPwr e1 e2)
          (urobAdd (urobMul (derive e2 x) (urobLn e1))
                   (urobDiv (urobMul e2 (derive e1 x)) e1))

derive (Ln e) x = urobMul (Pwr e (ICon (-1))) (derive e x)

urobAdd :: Exp -> Exp -> Exp
urobAdd (ICon x) (ICon y) = ICon (x+y)
urobAdd (Add e1 (ICon x)) (ICon y) = urobAdd (ICon (x+y)) e1
urobAdd (Add (ICon x) e1) (ICon y) = urobAdd (ICon (x+y)) e1
urobAdd (Add (ICon x) e1) (Add (ICon y) e2) = urobAdd (ICon (x+y)) (urobAdd e1 e2)
urobAdd e1 e2
  | e1 == (ICon 0) = e2
  | e2 == (ICon 0) = e1
  | e1 == e2 = urobMul (ICon 2) e1
  | otherwise = Add e1 e2

urobSub :: Exp -> Exp -> Exp
urobSub (ICon x) (ICon y) = ICon (x-y)
urobSub e1 e2
  | e1 == (ICon 0) = UMin e2
  | e2 == (ICon 0) = e1
  | e1 == e2 = ICon 0
  | otherwise = Sub e1 e2

urobMul :: Exp -> Exp -> Exp
urobMul (ICon x) (ICon y) = ICon (x*y)
urobMul (ICon x) (Mul (ICon y) e) = urobMul (ICon (x*y)) e
urobMul (Mul (ICon x) e1) (Mul (ICon y) e2) 
  = urobMul (ICon (x*y)) (urobMul e1 e2)
urobMul (Mul (ICon x) e1) e2 = urobMul (ICon x) (urobMul e1 e2)
urobMul e1 (Mul (ICon x) e2) = urobMul (ICon x) (urobMul e1 e2)
urobMul e1 (Pwr e2 e3) | e1 == e2 = urobPwr e1 (urobAdd (ICon 1) e3) 
urobMul (Pwr e1 e2)(Pwr e3 e4) | e1 == e3 = urobPwr e1 (urobAdd e2 e4)
urobMul (Mul e1 e2) e3 | e1 == e3 = urobMul e2 (urobPwr e1 (ICon 2))
                       | e2 == e3 = urobMul e1 (urobPwr e2 (ICon 2))
urobMul (Mul e1 e2) (Pwr e3 e4) | e1 == e3 = urobMul e2 (urobPwr e3 (urobAdd e4 (ICon 1)))
                                | e2 == e3 = urobMul e1 (urobPwr e3 (urobAdd e4 (ICon 1)))
urobMul (Pwr e1 e2) (Div e3 e4) | e1 == e4 = urobMul e3 (urobPwr e1 (urobSub e2 (ICon 1)))
urobMul (Div e3 e4) (Pwr e1 e2) | e1 == e4 = urobMul e3 (urobPwr e1 (urobSub e2 (ICon 1)))
urobMul (Pwr e1 e2) (Mul (Pwr e3 e4) e5)
  | e1 == e3 = urobMul (urobPwr e1 (urobAdd e2 e4)) e5
urobMul e1 e2
  | e1 == (ICon 0) = e1
  | e2 == (ICon 0) = e2
  | e1 == (ICon 1) = e2
  | e2 == (ICon 1) = e1
  | e1 == e2 = urobPwr e1 (ICon 2)
  | otherwise = Mul e1 e2
  
urobDiv :: Exp -> Exp -> Exp
urobDiv (Mul e1 e2) e3 | e1 == e3 = e2
                       | e2 == e3 = e1
urobDiv (Mul (Pwr e1 e2) e3) e4 | e1 == e4 = urobMul (urobPwr e1 (urobSub e2 (ICon 1))) e3
urobDiv (Mul e1 (Pwr e2 e3)) e4 | e2 == e4 = urobMul e1 (urobPwr e2 (urobSub e3 (ICon 1)))
urobDiv e1 e2
  | e1 == (ICon 0) = e1
  | e1 == (ICon 1) = e2
  | e2 == (ICon 1) = urobPwr e2 (ICon (-1))
  | e1 == e2 = (ICon 1)
  | otherwise = Div e1 e2

urobPwr :: Exp -> Exp -> Exp
urobPwr (ICon x) (ICon y) = (ICon (x^y))
urobPwr e (ICon x) | x == 0 = (ICon 1)
                   | x == 1 = e
urobPwr (Pwr e (ICon x)) (ICon y) =  urobPwr e (ICon (x*y))
urobPwr e1 e2 = Pwr e1 e2

urobLn :: Exp -> Exp
urobLn (Pwr e1 e2) = urobMul e2 (urobLn e1)
urobLn (ICon 1) = (ICon 0)
urobLn e = Ln e
-----------------
single :: Exp -> Bool
single (ICon _) = True
single (Var _) = True
single _ = False

expToStr' :: Exp -> Exp -> String -> String
expToStr' e1 e2 op 
  | (single e1) && (single e2) = expToStr e1 ++ op ++ expToStr e2
  | single e1 = expToStr e1 ++ op ++ "(" ++ expToStr e2 ++ ")"
  | single e2 = "(" ++ expToStr e1 ++ ")" ++ op ++ expToStr e2
  | otherwise = "(" ++ expToStr e1 ++ ")" ++ op ++ "(" ++ expToStr e2 ++ ")"

expToStr :: Exp -> String
expToStr (ICon x) = show x
expToStr (Var x)  = x
--expToStr (Add e1 e2) = "(" ++ expToStr e1 ++ " + " ++ expToStr e2 ++ ")"
--expToStr (Sub e1 e2) = "(" ++ expToStr e1 ++ " - " ++ expToStr e2 ++ ")"
expToStr (Add e1 e2) = expToStr e1 ++ " + " ++ expToStr e2 
expToStr (Sub e1 e2) = expToStr e1 ++ " - (" ++ expToStr e2 ++ ")"
expToStr (UMin e) = "-(" ++ expToStr e ++ ")"
expToStr (Mul e1 e2) = expToStr' e1 e2 "*"
expToStr (Div e1 e2) = expToStr' e1 e2 "/"
expToStr (Pwr e1 e2) = expToStr' e1 e2 "^"
expToStr (Ln e) = "Ln(" ++ expToStr e ++ ")"

d v p  =  putStrLn (expToStr (derive v p))
-----------------
c1 = ICon 1
cm1 = ICon (-1)
c2 = ICon 2
c3 = ICon 3
vx = Var "x"
vy = Var "y"
e3 = Add c1 c2  -- 2+3
e4 = Add c3 vx  -- 3+x 
e5 = Add c2 vy  -- 2+y
e6 = Add e3 e4  -- (2+3)+(3+x)
e7 = Add e4 e3  -- (3+x)+(2+3)
e8 = Add e4 e4  -- (3+x)+(2+y)
e9 = Sub e3 e6  -- 
e10 = Sub e9 e9 -- 0

e11 = Pwr vx vx
e12 = Add (Ln vx) c1
-- (x^x)*((Ln(x) + 1)^2)*((2*(x^-1))/(Ln(x) + 1))
e13 = Pwr e12 c2
e14 = Div (Mul c2 (Pwr vx cm1)) e12
e15 = derive (derive (derive e11 "x") "x") "x"
-- e15 = "(x^x)*((Ln(x) + 1)^3) + 2*((x^(x + -1))*(Ln(x) + 1)) + (x^(x + -1))*(Ln(x) + (x + -1)/x)"
e16 = Add vx c1
e17 = Pwr e16 e16