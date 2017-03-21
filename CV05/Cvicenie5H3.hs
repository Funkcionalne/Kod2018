module Cvicenie5H3 where

import Data.List
import Terms

instance Show LExp where
  show  = undefined
  
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
podtermy = undefined

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


