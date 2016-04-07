-- preliezanie vyrazov
-- lambda kodovanie logickych vyrazov
-- zoznamy
-- Y
-- stromy (este stale)
{-
logicke hodnoty

namiesto if P then Q else Read je COND P Q R
COND = \P.\Q.\R.(P Q R)
chceme aby (namiesto True budeme pisat len T a namiesto False len F)
COND T Q R -> Q
COND F Q R -> R 
takže T musí by funkcia \x.\y.x (vráti prvý argument) a 
F zasa \x.\y.y (vráti druhý argument)

COND TRUE A B =
(\p.\q.\r.(p q r))(\x.\y.x) A B =
(\q.\r.(\x.\y.x) q r) A B =
(\r.(\x.\y.x) A r) B =
(\x.\y.x) A B =
A (ako sme cakali)

teraz skusme OR, AND, NOT,...
hint: na co sa da mozeme pouzit COND
to je si dobre uvedomit:
pravda OR hocico = pravda
nepravda OR hocico = hocico

pravda AND hocico = hocico
nepravda AND hocico = nepravda

Najlahsi asi bude NOT, ked vyuzijeme COND
NOT X = COND X (NOT X) X - rekurzia :-(, 
ale hneï si uvedomíme, že X je buï T alebo F
teda h¾adáme funkciu, ktorá keï dostane T vráti F a naopak - no samozrejme NOT
NOT x = \x.(x F T)

teraz mozeme skúsi napr. AND
AND AB = COND A B FALSE  (to sme len prepísali, èo platí pre AND)
dosadíme
\A.\B.((\p.\q.\r.p q r) A B F) =
\A.\B.((\q.\r.A q r) B F) =
\A.\B.(A B F)

a OR analogicky
OR A B = COND A T B
dosadíme
\A.\B.((\p.\q.\r.p q r) A T B) =
\A.\B.((\q.\r.A q r) T B) =
\A.\B.((\r.A T r) B) =
\A.\B.(A T B)

môžeme skúsi
T AND F =
\A.\B.(A B F) T F =
T F F = 
dosadíme aj za T a F
(\x.\y.x) (\x.\y.y) (\x.\y.y) =
(\y.(\x.\y.y))(\x.\y.y) =
(\x.\y.y) 
èo je ozaj F, ako oèakávame

T AND T =
\A.\B.(A B F) T T =
T T F = 
(\x.\y.x) (\x.\y.x) (\x.\y.y) =
(\y.(\x.\y.x))(\x.\y.y) =
(\x.\y.x) èo je T.

OR si môžete skúsi

zoznamy

zaujíma nás:
 èo je prázdny zoznam NIL ([])?
 èo je konštruktor vytvárania zoznamu CONS (:)?
 
vieme, že pre zoznam platí CONS A B je neprázdny zoznam
a HD (CONS A B) = A a TL (CONS A B) = B,
kde HD je hlava (prvý prvok) zoznamu a TL chvost (zvyšok) zoznamu
CONS teda musí vráti funkciu, ktorá oèakáva hlavu a chvost zoznamu a ešte aj selektor, 
ktorý sa aplikuje v na vytvorený zoznam niekdy v budúcnosti.
teda
CONS je \h.\t.\s.(s h t)
takže selektor vyberie buï h alebo t. Ale veï to predsa robí presne T a F.
HD = \L.(L T)
TL = \L.(L F)

skúsime
HD (CONS A B) =
dosadíme
(\L.(L T))((\h.\t.\s.(s h t)) A B) =
((\t.\s.(s A t)) B) T =
(\s.(s A B)) T =
T A B =
A, ako sme aj oèakávali

ešte nám ostal test èi je zoznam prázdny (NULL) a definícia prázdneho zoznamu (NIL).
NULL (CONS A B) musí ma hodnotu F
rozpíšeme a pokúsime sa zjednoduši
NULL ((\h.\t.\s.(s h t)) A B) =
NULL (\s.(s A B))
NULL musí teda by funkcia, ktorá keï dostane (\s.(s A B)), vráti F.
Nemôže to by \x.F, lebo by nerozlišovala èi dostane prázdny alebo neprázdny zoznam.
Takže musí nejako zmysluplne spracova argument. 
Napríklad takto:
(podobne ako zoznamové selektory)
NULL=\L.L(\h.\t.F), teda ak dostane zoznam vytvorený konštruktorom CONS použije špecialny selektor, 
ktorý "ignoruje" hlavu aj chvost zoznamu a vráti F, lebo zoznam bol neprázdny.

A ako bude vyzera prázdny zoznam NIL?
NULL NIL má by T
ale už vieme èo je NULL, tak môžeme dostadi:
\L.L(\h.\t.F) NIL
Teda NIL má by funkcia ktorú ak aplikujeme na (\h.\t.F) dostaneme T.
Priamoèiare riešenie je argument ignorova a vráti T
Teda
NIL = \x.T

príklad:
na lambdy rozpíšeme až keï bude treba, kvôli preh¾adnosti
NULL (TL (CONS A NIL)) =
(\L.L(\h.\t.F)) (TL (CONS A NIL)) =
(TL (CONS A NIL)) (\h.\t.F) =
((\L.(L F))(CONS A NIL)) (\h.\t.F) =
((CONS A NIL) F) (\h.\t.F) =
(((\h.\t.\s.(s h t)) A NIL) F) (\h.\t.F) =
((\s.(s A NIL)) F) (\h.\t.F) =
(F A NIL) (\h.\t.F) =
((\x.\y.y) A NIL) (\h.\t.F) =
NIL (\h.\t.F) =
(\x.T) (\h.\t.F) =
T, ako sme aj oèakávali.

-}


--rozcvicka
import TypyTree
-- module TypyTree where
-- data Tree a = Empty | Branch a (Tree a) (Tree a)
--   deriving (Read, Show)
  
-- Napíšte funkciu jeVyvazeny :: Tree a -> Bool
-- ktorá vráti True, ked sa výška žiadnych dvoch bratov nelíši viac než o 1. 
-- Inak vráti False.  
jeVyvazeny :: Tree a -> Bool
jeVyvazeny = fst . jeVyvazeny'

jeVyvazeny' :: Tree a -> (Bool, Int)
jeVyvazeny' Empty = (True, 0)
jeVyvazeny' (Branch _ t1 t2) = (r1 && r2 && abs(h1 - h2) < 2, max h1 h2 + 1)
  where (r1, h1) = jeVyvazeny' t1
        (r2, h2) = jeVyvazeny' t2

s1 = Empty
s2 = Branch 1 s1 s1
s3 = Branch 2 s1 s2
s4 = Branch 3 s1 s2        
s5 = Branch 4 s2 s3
s6 = Branch 5 s4 s5
s7 = Branch 7 s1 s6 -- nevyvazeny
s8 = Branch 8 s6 s5
s9 = Branch 9 s8 s5 -- nevyvazeny
s10 = Branch 10 s9 s7 -- nevyvazeny
