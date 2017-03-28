module Cvicenie5H3 where

import Data.List
import Terms

-- LAMBDA Var LExp | ID Var | APP LExp LExp
{-
instance Show LExp where
  show (LAMBDA x1 ex) = "(\\" ++ x1 ++ " -> " ++ (show ex) ++ " )"
  show (ID x1) = x1
  show (APP ex1 ex2) = "( " ++ (show ex1) ++ " " ++ (show ex2) ++" )"
-}  
  
ione =    (APP isucc izero)
itwo =    (APP isucc (APP isucc izero))
ifour =   (APP isucc (APP isucc (APP isucc (APP isucc izero))))
ieight =  (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc izero))))))))

izero = (LAMBDA "f" (LAMBDA "x" (ID "x")))
omega = (LAMBDA "x" (APP (ID "x") (ID "x")))
isucc = (LAMBDA "n" 
          (LAMBDA "f" 
            (LAMBDA "x" (APP (ID "f") (APP (APP (ID "n") (ID "f")) (ID "x"))) )))

-- najst vsetky podtermy termu
-- priamociaro
podtermy :: LExp -> [LExp]
podtermy x@(LAMBDA x1 ex) = [x]  ++ (podtermy ex)
podtermy x@(ID x1) = [x]
podtermy x@(APP ex1 ex2) = [x]  ++ (podtermy ex1) ++ (podtermy ex2)

podtermy' x = nub $ podtermy x

foldlExp:: (String -> b) -> (b->b->b) -> (String -> b -> b) -> LExp -> b
foldlExp var app lambda (ID x1) = var x1
foldlExp var app lambda (APP ex1 ex2) = app (foldlExp var app lambda ex1) (foldlExp var app lambda ex2)
foldlExp var app lambda (LAMBDA x1 ex) = lambda x1 (foldlExp var app lambda ex)

instance Show LExp where
	show ex = foldlExp id (\x -> \y -> "(" ++ x ++ " " ++ y ++ ")") 
					    (\var -> \y -> "(\\" ++ var ++ " ->" ++ y ++ ")")
						ex
{-						
podtermy'' :: LExp -> [LExp]
podtermy'' ex = foldlExp (\x -> [ID x]) (\x -> \y -> x ++ y ++ )
-}

subst :: LExp -> String -> String -> LExp
subst (ID x1) var1 var2 = if x1 == var1 then (ID var2) else (ID x1)
subst (APP ex1 ex2) var1 var2 = (APP (subst ex1 var1 var2) (subst ex2 var1 var2))
subst (LAMBDA x ex) var1 var2 = if x == var1 then (LAMBDA x ex) else pom
									where pom = if (freeVar ex var1) then (LAMBDA (x++x) (subst (subst ex x (x++x)) var1 var2)) else (LAMBDA x (subst ex var1 var2))

freeVar :: LExp -> String -> Bool
freeVar (ID x) var = x == var
freeVar (APP ex1 ex2) var = (freeVar ex1 var) || (freeVar ex2 var)
freeVar (LAMBDA x ex) var = if x == var then False else freeVar ex var
{--
podtermy (LAMBDA "x" (APP (ID "x") (ID "x")))
[\x->(x x),(x x),x]
--}

-- akumulatorom

rovnake :: LExp -> LExp -> Bool
rovnake (LAMBDA v1 e1) (LAMBDA v2 e2) = (v1 == v2) && rovnake e1 e2
rovnake (ID v1) (ID v2) = v1 == v2
rovnake (APP e11 e21) (APP e12 e22) = (rovnake e11 e12) && (rovnake e12 e22)
rovnake _ _ = False

----

-- nahradi premennu za premennu
nahrad ::  LExp -> String -> String -> LExp
nahrad = undefined

{-
nahrad (LAMBDA "x" (APP (ID "x") (ID "x"))) "x" "y"
\x->(x x)
nahrad (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "z"
\x->(x z)
nahrad (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "x"
\x->(x x)
:-(
-}

free    :: String -> LExp -> Bool
free =   undefined


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
alpha  = undefined

e1 = [
  alpha (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "x" (APP (ID "x") (ID "y")))
  ,
  alpha (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "y" (APP (ID "y") (ID "y")))
  ,
  alpha (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "y" (APP (ID "z") (ID "y")))
  ]
