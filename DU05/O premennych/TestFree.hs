-- kodovanie UTF-8 bez BOM (Notepad++)
module Main where

import Terms
import qualified Beta as F
import Test.HUnit

main = do
  runTestTT $  
    TestList [
      TestList [ 
            TestCase $ assertEqual ("free " ++ (show t))
                                   (free t)
                                   (F.free t) | t<- alls]
     ]                              
------------
alls = [omega, isucc, iplus, itimes, ipower, true, false, land, 
        lor, lnot, lxor, y, isZero, pair, first, second]

vars = ["x", "y", "z"]        

omega = fromString "λx.(x x)"
isucc  = fromString "λn.λf.λx.(f ((n f) x))"
iplus =  fromString "λm.λn.λf.λx.((m f) ((n f) x))" 
itimes = fromString "λm.λn.λf.λx.((m (n f)) x)"
ipower = fromString "λm.λn.(n m)"
true =  fromString "λx.λy.x"
false = fromString "λx.λy.y"
land =  fromString "λx.λy.((x y) FALSE)"
lor =   fromString "λx.λy.((x TRUE) y)"
lnot =  fromString "λx.((x FALSE) TRUE)" 
lxor =  fromString "λx.λy.((x ((y FALSE) TRUE)) ((y TRUE) FALSE))"
y =     fromString "λf.(λx.(f (x x)) λx.(f (x x)))"
isZero =fromString "λn.((n λy.FALSE) TRUE)"
pair =  fromString "λx.λy.λc.((c x) y)"
first = fromString "λx.(x TRUE)"
second =fromString "λx.(x FALSE)"

 
------- riesenie Mario Lipovsky, 2015

same :: LExp -> LExp -> Bool
same (ID _) (ID _)       = True
same (LAMBDA _ e1) (LAMBDA _ e2) = same e1 e2
same (APP e1 e2) (APP f1 f2)  = same e1 f1 && same e2 f2
same _ _ = False

-- `subterms l` vr�ti zoznam v�etk�ch podtermov termu `l`
subterms :: LExp -> [LExp]
subterms (ID x)       = [ID x]
subterms (LAMBDA x e) = (LAMBDA x e):(subterms e)
subterms (APP e1 e2)  = ((APP e1 e2):(subterms e1)) ++ (subterms e2)

-- `free l` vr�ti zoznam v�etk�ch premenn�ch, ktor� maj� vo�n� v�skyt v terme `l`
free :: LExp -> [Var]
free (ID x)       = [x]
free (LAMBDA x e) = filter (/=x) (free e)
free (APP e1 e2)  = (free e1) ++ (free e2)
 
-- `bound l` vr�ti zoznam v�etk�ch premenn�ch, ktor� maj� viazan� v�skyt v terme `l`
bound :: LExp -> [Var]
bound (ID x)       = []
bound (LAMBDA x e) = x:(bound e)
bound (APP e1 e2)  = (bound e1) ++ (bound e2)
 
-- `substitute v k l` substituuje v�etky vo�n� v�skyty `v` v terme `l` za `k`
substitute ::  Var -> LExp -> LExp -> LExp
substitute x k (ID y)
  | x == y    = k
  | otherwise = ID y
substitute x k (APP e1 e2) = APP (substitute x k e1) (substitute x k e2)
substitute x k (LAMBDA y e)
  | x == y = LAMBDA y e
  -- nove mena premennych budu x0, x1, x2 ... take, co nie su volne v e, /= x, /= y
  | (x `elem` free(e)) && (y `elem` free(k)) = LAMBDA w (substitute x k (substitute y (ID w) e))
  | otherwise = LAMBDA y (substitute x k e)
    where
      w = head [ nova | nova <- map (('x':).show) [0..], nova `notElem`  (free e), nova /= x, nova /=y] 

-----------------------------------------------------------------------------------------------
      
-- `betaReduce l` sprav� nejak� beta redukcie v `l`
-- pod�a Vami zvolenej strat�gie
      
betaReduce :: LExp -> LExp
betaReduce (ID x)                 = ID x
-- eta redukcia
--betaReduce (LAMBDA x (APP e y))
-- | (ID x)==y && (x `elem` free(e)) == False = betaReduce e
-- | otherwise                                = LAMBDA x (betaReduce (APP e y))
betaReduce (LAMBDA x e)           = LAMBDA x (betaReduce e)
betaReduce (APP (LAMBDA x e1) e2) = substitute x e2 e1
betaReduce (APP e1 e2)            = APP (betaReduce e1) (betaReduce e2)

-- `normalForm l` iteruje `betaReduce` na `l`, k�m sa men� 
normalForm :: LExp -> LExp
normalForm e = normalForm' e (betaReduce e)
  where
    normalForm' e1 e2
      | e1 == e2  = e2
      | otherwise = normalForm' e2 (betaReduce e2)
      
------------------------------------------------ 

splitOn :: Char -> String -> (String, String)
splitOn c xs = splitOn' [] xs c 0
  where
    splitOn' xs [] c n    = (reverse xs,[])
    splitOn' xs (y:ys) c n
      | y==c && n==0 = (reverse xs, ys)
      | y=='('       = splitOn' (y:xs) ys c (n+1)
      | y==')'       = splitOn' (y:xs) ys c (n-1)
      | otherwise    = splitOn' (y:xs) ys c n
  
fromString :: String -> LExp
fromString str 
  | str `elem` fbase = snd (head [x | x<-base, fst x == str ]) 
  | (head str == 'λ') || (head str == '\\') = LAMBDA (fst rozdel_b) (fromString (snd rozdel_b))
  | head str == '('  = APP (fromString (fst rozdel_m))  (fromString (snd rozdel_m))
  | otherwise        = ID str
    where 
      rozdel_b = splitOn '.' (tail str)
      rozdel_m = splitOn ' ' (tail (init str))
      fbase = map fst base
 
church 0 = fromString "λf.λx.x"
church n = normalForm( APP (fromString "succ") (church (n-1)))      
      
base=[
  ("i", LAMBDA "x" (ID "x")),
  ("k", LAMBDA "x" (LAMBDA "y" (ID "x"))),
  ("s", LAMBDA "x" (LAMBDA "y" (LAMBDA "z" (APP (APP (ID "x") (ID "z")) (APP (ID "y") (ID "z")))))),
  ("omega", fromString "λx.(x x)"),
    
  ("succ",  fromString "λn.λf.λx.(f ((n f) x))"),
  ("+",     fromString "λm.λn.λf.λx.((m f) ((n f) x))"), 
  ("*",     fromString "λm.λn.λf.λx.((m (n f)) x)"),
  ("^",     fromString "λm.λn.(n m)"),
    
  ("TRUE",  fromString "λx.λy.x"),
  ("FALSE", fromString "λx.λy.y"),
  ("AND",   fromString "λx.λy.((x y) FALSE)"),
  ("OR",    fromString "λx.λy.((x TRUE) y)"),
  ("NOT",   fromString "λx.((x FALSE) TRUE)"), 
  ("XOR",   fromString "λx.λy.((x ((y FALSE) TRUE)) ((y TRUE) FALSE))"),
  
  ("Y",     fromString "λf.(λx.(f (x x)) λx.(f (x x)))"),
  
  ("isZero",fromString "λn.((n λy.FALSE) TRUE)"),
  
  ("PAIR",  fromString "λx.λy.λc.((c x) y)"),
  ("FST",   fromString "λx.(x TRUE)"),
  ("SND",   fromString "λx.(x FALSE)"),
  
  -- creates list (with pairs) of size n+1, with n*f and 1*x  
  --("fList", fromString "λn.λf.(n λx.((PAIR f) x))"),
  -- removes 1st element of list, then applies 1st pair element on 2nd element recursively
  --("pred'",  fromString "λn.λf.λg.((Y λA.λp.((FST p) (A (SND p)))) (SND (((fList n) f) ((PAIR λx.g) 0))))"),
  --("pred",   fromString "λn.(((isZero n) 0) (pred' n))"),
  ("pred",   fromString "λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)"),
    
  -- n-krat aplikuje predecessora na m
  ("-",   fromString "λm.λn.((n pred) m)"),
  -- return n-m != 0 (v skutocnosti (n-m <= 0)' <=> (n<=m)' <=> n>m 
  ("<",    fromString "λm.λn.(NOT (isZero ((- n) m)))"),
  
  -- skus(o,m,n) = IF((o+1)*n > m) o ELSE skus(o+1,m,n)   
  ("/", fromString "((Y λskus.λo.λm.λn.((((< m) ((* (succ o)) n)) o) (((skus (succ o)) m) n))) 0)"),
  -- return m - n*(m div n)
  ("%", fromString "λm.λn.((- m) ((* n) ((/ m) n)))"),
  
  
  ("sum",   fromString "(Y λS.λn.(((isZero n) 0) ((+ n) (S (pred n)))))"),  -- do 4 s rek. pred / inak aj 15
  ("fac",   fromString "(Y λF.λn.(((isZero n) 1) ((* n) (F (pred n)))))")   -- do 3 s rek. pred / 
  
  ] ++ (take 20 [(show n, church n) | n <- [0..]])
 
 
calc x = normalForm (fromString x)