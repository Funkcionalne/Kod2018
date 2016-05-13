-- cvicenie 11
import Prelude hiding ((<*>), (<|>))
import Data.Char

type Parser symbol result = [symbol] -> [([symbol],result)]

symbola :: Parser Char Char
symbola [] = [] -- ak nie je nič na vstupe
symbola (x:xs) | x=='a' = [ (xs, 'a') ] -- ak je 'a' na vstupe
               | otherwise= [] -- ak nie je 'a' na vstupe

symbol :: Eq s => s -> Parser s s
symbol a [] = []
symbol a (x:xs) | a==x = [ (xs, x) ]
                | otherwise= []

symbol' :: Eq s => s -> Parser s s
symbol' a [] = []
symbol' a (x:xs) = [ (xs, a) | a == x ]

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

satisfy :: (s -> Bool) -> Parser s s
satisfy p [] = []
satisfy p (x:xs) = [ (xs, x) | p x ]

symbol'' a = satisfy (\x->x==a)
symbol''' a = satisfy (==a)

digit10 :: Parser Char Char
digit10 = satisfy isDigit

hexa :: Parser Char Char
hexa = satisfy (\x->elem x "0123456789ABCDEF")

hexa' = satisfy (\x-> isDigit x || elem x ['A'..'F'])
hexa'' =satisfy isHexDigit

epsilon :: Parser s () -- () je ako typ void
epsilon xs = [ ( xs, () ) ] -- () hodnota typu (), ako null

fail :: Parser s r
fail xs = []

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

yes = token "YES"
no = token "NO"
yesno = yes <|> no

-- toto asi nie, kvoli typom
-- no' = symbol 'N' <*> symbol 'O'
-- yes' = symbol 'Y' <*> symbol 'E' <*> symbol 'S'
-- yesno' = yes' <|> no'

sp :: Parser Char a -> Parser Char a
sp p = p . dropWhile (==' ')

just :: Parser s a -> Parser s a
just p = filter (null.fst) . p

------- odtialto je to cvicenie 12

--cv.6
just' p xs = [x | x <- p xs, null $ fst x]

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
-- p <:**> q = p <*> q  <@ (\x->(fst x):(snd x))
-- alternativa 
p <:**> q = p <*> q <@ uncurry (:)
-- da sa aj cez where a <* a *>
-- alebo aj takto
-- p <:**> q xs = [(ys, z:zs) | (xs2,z) <- p xs, (ys, zs) <- q xs2]

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
sequence' [] = succeed []
sequence' (x:xs) = x <:*> (sequence' xs)

--cv 8', v subore s kodom z prednasky\
token' :: Eq s => [s] -> Parser s [s]
token' = sequence' . map symbol

--cv 9.
mobOk = sequence' ([symbol '0', symbol '9'] ++ (replicate 8 digit10))

choice :: [Parser s a] -> Parser s a
choice = foldr (<|>) failp