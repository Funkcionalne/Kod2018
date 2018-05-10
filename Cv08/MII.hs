module Cvicenie08 where
import Prelude hiding ((<*>), (<|>), (*>), (<*), sequence)
import Data.Char

type Parser symbol result = [symbol] -> [([symbol],result)]

symbola :: Parser Char Char
symbola [] = [] -- ak nie je nič na vstupe
symbola (x:xs) | x=='a' = [ (xs, 'a') ] -- ak je 'a' na vstupe
               | otherwise= [] -- ak nie je 'a' na vstupe

-- to iste ale parametrizovane
symbol :: Eq s => s -> Parser s s
symbol a [] = []
symbol a (x:xs) | a==x = [ (xs, x) ]
                | otherwise= []

-- inak zapisane
symbol' :: Eq s => s -> Parser s s
symbol' a [] = []
symbol' a (x:xs) = [ (xs, a) | a == x ]

-- odreze prefix ak je rovnaky
token :: Eq s => [s] -> Parser s [s]
token k xs | k == take n xs = [ (drop n xs, k)]
           | otherwise = []
   where n = length k

k0 = []
k1 = ["kuk"]
k2 = ["ahoj", "kuk"]
k3 = ["ahoj"]

k4 = "kuk"
k5 = "ku"
k6 = "u"

--vseobecnejsi symbol, 
satisfy :: (s -> Bool) -> Parser s s
satisfy p [] = []
satisfy p (x:xs) = [ (xs, x) | p x ]

-- symbol inak, pomocou satisfy, cv. 1.
-- Keďže satisfy je zovšeobecnením symbol, definujte symbol ako inštanciu satisfy.

symbol'' :: Eq s => s -> Parser s s
symbol'' a = satisfy (==a)

--cv. 2, a 3.
digit10 :: Parser Char Char
digit10 = satisfy (isDigit)

-- Definujte analyzátor hexa::Parser Char Char, ktorý analyzuje jednu šesnástkovú cifru, t.j. (0, 1, …, F)
hexa :: Parser Char Char
hexa = undefined

hexa' = satisfy (\x-> isDigit x || elem x ['A'..'F'])
hexa'' =satisfy isHexDigit

epsilon :: Parser s () -- () je ako typ void
epsilon xs = [ ( xs, () ) ] -- () hodnota typu (), ako null

failp :: Parser s r
failp xs = []

succeed :: r -> Parser s r
succeed v xs = [ (xs, v) ]

infixr 6 <*> -- sekvenčné zreťazenie analyzátorov
infixr 4 <|>

(<*>) :: Parser s a -> Parser s b -> Parser s (a,b)
(p1 <*> p2) xs = [ (xs2, (v1,v2))
                    | (xs1, v1) <- p1 xs,
                      (xs2, v2) <- p2 xs1
                 ]
(<|>) :: Parser s a -> Parser s a -> Parser s a
(p1 <|> p2) xs = p1 xs ++ p2 xs

-- cv.5
yes = token "yes"
no = token "no"
yesno = yes <|> no

-- toto asi nie, kvoli typom
-- no' = symbol 'N' <*> symbol 'O' -- <*> succeed "NO"
-- yes' = symbol 'Y' <*> symbol 'E' <*> symbol 'S' -- <*> succeed "YES"
-- yesno' = yes' <|> no'

sp :: Parser Char a -> Parser Char a
sp p = p . dropWhile (==' ')

just :: Parser s a -> Parser s a
just p = filter (null.fst) . p

------- 

--cv.6
just' p xs = [(ys, v) | (ys, v) <- p xs, ys == []]

infix 5 <@ -- infixová notácia pre aplikátor
(<@) :: Parser s a -> (a->b) -> Parser s b
(p <@ f) xs = [ (ys, f v) | (ys, v) <- p xs ]

infixr 6 <*
(<*) :: Parser s a -> Parser s b -> Parser s a
p <* q = p <*> q <@ fst
 
infixr 6 *>
(*>) :: Parser s a -> Parser s b -> Parser s b
p *> q = p <*> q <@ snd

list :: (a, [a]) -> [a]
list (x, xs) = x:xs

infixr 6 <:*>
(<:*>) :: Parser s a -> Parser s [a] -> Parser s [a]
p <:*> q = p <*> q <@ list

-- cv 7.
infixr 6 <:**>
(<:**>) :: Parser s a -> Parser s [a] -> Parser s [a]
p <:**> q = p <*> q <@ (uncurry (:))

single :: a -> [a]
single x = [x]
single' = \x -> [x]

-- {p}
option :: Parser s a -> Parser s [a]
option p = p <@ single <|> succeed []

-- {p}^*
many :: Parser s a -> Parser s [a]
many p = p <:*> (many p) <|> succeed []

-- {p}^+
many1 :: Parser s a -> Parser s [a]
many1 p = p <:*> many p

sequence :: [Parser s a] -> Parser s [a]
sequence = foldr (<:*>) (succeed [])

--cv 8.
sequence' :: [Parser s a] -> Parser s [a]
sequence' [] = succeed []
sequence' (p:ps) = p <:*> (sequence' ps)

--cv 8', v subore s kodom z prednasky\
token' :: Eq s => [s] -> Parser s [s]
token' = sequence' . map symbol

--cv 9.

mobOk = sequence ([ symbol '0', symbol '9'] ++ (replicate 8 digit10))

choice :: [Parser s a] -> Parser s a
choice = foldr (<|>) failp

psc  :: Parser Char [Char]
psc = undefined

-- cv. 10
year  :: Parser Char [Char]
year = undefined

--month  :: Parser Char (Char,Char)
month = sequence ([ symbol '0', digit10]) 
    <|> sequence ([ symbol '1', satisfy (\a -> elem a "012")])
        
month'  :: Parser Char [Char]
month' = undefined

day  :: Parser Char (Char,Char)
day = undefined
        
day'  :: Parser Char [Char]
day' = undefined
         
day''  :: Parser Char [Char]
day'' = undefined
         
--------------------
-- Determinsm

determ :: Parser a b -> Parser a b
determ p xs  |  null r     =  []
             |  otherwise  =  [head r]
                     where r = p xs

compulsion = determ . option

greedy = determ . many

greedy1 = determ . many1
  
-- Some common special cases


identifier  ::  Parser Char String
identifier   =  satisfy isAlpha <:*> greedy (satisfy isAlphaNum)

digit       ::  Parser Char Int
digit        =  satisfy isDigit  <@  f
         where  f c = ord c - ord '0'


natural     ::  Parser Char Int
natural      =  greedy1 digit  <@  foldl f 0
         where  f a b = a*10 + b

integer     ::  Parser Char Int
integer      =  option (symbol '-') <*> natural  <@  f
         where  f ([],n) =  n
                f (_ ,n) =  -n
                
--------------------
-- cv. 11         
natural'  :: Parser Char Int
natural' = (many digit) <@ foldr (\y -> \x -> 10*x + y) 0 . reverse
         
         
-- cv.12

(<?@)        ::  Parser s [a] -> (b,a->b) -> Parser s b
p <?@ (b,g)   =  p <@ f
          where  f []  = b
                 f [h] = g h               
  
{-
"?: " fixed "12.3456"
[("",12.3456),(".3456",12.0)]
-}

fixed       ::  Parser Char Float
fixed        =  (integer <@ fromIntegral)
                <*> 
                (option (symbol '.' *> fractpart)  <?@  (0.0,id))
                <@  uncurry (+)

{-
"?: " fractpart "3123123"
[("",0.3123123)]
-}

fractpart   ::  Parser Char Float
fractpart    =  greedy digit  <@  foldr f 0.0
         where  f d n = (n +  fromIntegral d)/10.0
       
{-
"?: " fixed "12.3456"
[("",12.3456),(".3456",12.0)]
-}		
float       ::  Parser Char Float
float        =  fixed 
                <*> 
                (option (symbol 'E' *> integer) <?@ (0,id) )
                <@ f
         where  f (m,e)  =  m * power e
                power e | e<0       = 1.0 / power (-e)
                        | otherwise = fromInteger(10^e)
         
------------------------------------------------------------------------         
         
-- V -> a V a | ε
parserV :: Parser Char [Char]
parserV = (symbol 'a' <*> parserV <*> symbol 'a') <@ (\(x,(y,z)) -> x:(y++[z]))
         <|> succeed []
                          
justparserV :: Parser Char [Char]
justparserV = just $ parserV
                                          
--------------------
-- S -> S S | a
{-- zle riesenie}               
parserS :: Parser Char [Char]
parserS = ( parserS <*> parserS ) <@ (\(x,y) -> (x++y))
          <|>
          succeed []
---}

parserS :: Parser Char [Char]
parserS = undefined
        
justparserS :: Parser Char [Char]
justparserS = just $ parserS

--------------------
-- R -> a R a | b R b | c R c | ε
parserR :: Parser Char [Char]
parserR = undefined
                          
justparserR :: Parser Char [Char]
justparserR = just $ parserR
