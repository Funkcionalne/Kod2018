module Error where

{-
instance (Error e) => Monad (Either e) where  
        return x        = Right x   
        Right x >>= f   = f x  
        Left err >>= f  = Left err  
-}

data Term = Con Int | 
      Div Term Term |
      Add Term Term |
      Sub Term Term |
      Mult Term Term 
      deriving(Show, Read, Eq)

eval          :: Term -> Either String Int 
eval(Con a)   = return a
eval(Add t u) = do v <- eval t
                   w <- eval u
                   return (v+w)
eval(Mult t u) = do v <- eval t
                    w <- eval u
                    return (v*w)
                   
eval(Div t u) = do v <- eval t
                   w <- eval u
                   if w == 0 then (Left "div by zero")
                   else return (v `div` w)

q = Add (Con 23) (Con 4)
 
t :: Term
t = Div (Con 1972) (Con 23) 

t'' = Add (Div (Con 1972) (Con 23)) (Con 4)

t' :: Term
t' = Div (Con 1972) (Con 0) 

{-
"?: " eval t
Right 85
"?: " eval t'
*** Exception: div by zero
-}
