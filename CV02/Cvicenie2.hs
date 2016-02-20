-- =========================================== STATISTIKY
-- zapnite si statistiku direktivou
-- :set +s

{- standardny length
"?: " length [1..10000000]
10000000
(0.28 secs, 803,575,888 bytes)

"?: " length [1..20000000]
20000000
(0.45 secs, 1,598,085,792 bytes)
-}

-- klasicka rekurzia - z rekurzivneho volania sa MUSIME 
-- vratit a nieco vykonat 1 + vysledok

dlzka :: [t] -> Int
dlzka []  = 0
dlzka (x:xs) = 1 + dlzka xs

{-
"?: " dlzka [1..10000000]
10000000
(5.76 secs, 2,266,204,000 bytes)

"?: " dlzka [1..20000000]
*** Exception: stack overflow
-}

---------------
-- chvostova rekurzia - z rekurzivneho volania sa nemusime vratit
-- sikovny kompilator z toho urobi GOTO

dlzka' :: [t] -> Int
dlzka' xs = dlzka_ xs 0
  where dlzka_ [] acc     = acc
        dlzka_ (x:xs) acc = dlzka_ xs (acc+1)   -- accumulator je od akumulovat hodnotu...
        
{-
"?: " dlzka' [1..10000000]
10000000
(8.49 secs, 2,657,135,568 bytes)

"?: " dlzka' [1..20000000]
*** Exception: stack overflow
-}        
---------------
dlzka'' :: [a] -> Int
dlzka'' xs = dlzka_ (0,xs)
    where dlzka_ (n,[]) = n
          dlzka_ (n,x:xs) = dlzka_ (n+1,xs)
{- 
"?: " dlzka'' [1..10000000]
10000000
(8.83 secs, 2,898,088,608 bytes)

"?: " dlzka'' [1..20000000]
*** Exception: stack overflow
-}
---------------

{-
built-in
"?: " last $ reverse [1..100000]
(0.02 secs, 11,652,528 bytes)
"?: " last $ reverse [1..1000000]
(0.16 secs, 102,573,272 bytes)
-}

-- naivna implementacia
reverse'  :: [t] -> [t]
reverse'  []  = []
reverse'  (x:xs)  = reverse' xs ++ [x]

{-
"?: " last $ reverse' [1..100000]
(1051.20 secs, 444,465,831,232 bytes)

-- !!!! poucenie : alokovanie pamati je zdaleka brutalne najdrahsia operacia pri vypocte
-}

-- naivna implementacia
reverse''  :: [t] -> [t]
reverse''  xs  = loop xs []
    where loop [] acc     = acc
          loop (x:xs) acc = loop xs (x:acc)
{-
last $ reverse'' [1..100000]
(0.11 secs, 36,254,432 bytes)
-}
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- ---------------------- LIST COMPREHENSION -----------------------------
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------

-- neparne cisla <= 100
e1  :: [Int]
e1 = [1,3..100]

e2  :: [Int]
e2 = [ 2*x+1 | x <-[0..100], 2*x+1 <= 100]

-- dvojica: (neparne cislo, boolean)
e3  :: [(Int,Bool)]
e3 = [ (2*x+1,b) | x <-[0..100], 2*x+1 <= 100, b <- [True, False] ]

-- zmena poradia v list-comprehension
e4  :: [(Int,Bool)]
e4 = [ (2*x+1,b) | x <-[0..100], b <- [True, False], 2*x+1 <= 100 ]

-- vnoreny list-comprehension
e5  :: [ [ (Int, Bool) ]]
e5 = [ [(2*x+1,b) | b <- [True, False] ] | x <-[0..100], 2*x+1 <= 100 ]

e6  :: [(Int,Bool)]
e6 = concat e5

-- e6 == e4 ????????????

-- ----------------------------------------------------------------

-- nekonecny zoznam prirodzenych cisel
n1  :: [Int]
n1 = take 10 [0..]

-- dvojice prirodzenych cisel
n2  :: [(Int, Int)]
n2  = [(a, b) | a <- n1, b <- n1]
-- ale je ich len 100

-- preco nemozeme urobit toto, ak chceme dostat vsetky dvojice
n3  :: [(Int, Int)]
n3  = [ (a, b) | a <- [0..], b <- [0..] ]

-- take 1000 n3 ?????????

-- lepsie takto
n4  :: [(Int, Int)]
n4  = [ (sucet-b, b) | sucet <- [0..], b <- [0..sucet] ]

-- take 1000 n4 ?????????

--------------------------------------------------------------------------

{- Inverzna funckia
v celom zadani predpokladame, ze f :: Float->Float je monotonna - preco asi ??
ak f x = y, my hladame inverznu funkciu g, taku, ze g y = x
vlastne hladame koren f x - y = 0

ludsky: dostaneme y, a hladame take x, ze f x == y
- skusime x1 = -9999999999 x2 = 9999999999
- ak Q1 = f x1 - y, Q2 = f x2 - y maju rovnake znamienko, tak koren neexistuje
- ak Q1 = f x1 - y, Q2 = f x2 - y maju rozne znamienko, tak koren existuje medzi x1 a x2
- zoberieme x3 = (x1+x2)/2 a vypocitame Q3 = f x3 - y
- z dvojic (x1, x3) a (x3, x2) zoberieme tu, kde su ROZNE znamienka pre (Q1, Q3) ci (Q3, Q2)
- opakujeme kym x1 a x2 nie su dost blizko
- Newton to vylepsil derivaciou a vola sa to Newtonova metoda hladania korena
- https://en.wikipedia.org/wiki/Newton's_method

-}

inverzna  :: (Float->Float) -> (Float->Float)
inverzna f = \y -> newton y (-9999999999) 9999999999
        where 
            epsilon = 0.0001 
            newton y x1 x2 = if abs(x1-x2) < epsilon then 
                              (x1+x2)/2
                           else let x3 = (x1+x2)/2 in
                                    if (f x1 - y) * (f x3 - y) < 0 then -- maju rozne znamienka
                                      newton y x1 x3
                                    else if (f x3 - y) * (f x2 - y) < 0 then
                                      newton y x3 x2
                                    else
                                      error ("something's wrong: " ++ show x1 ++ "," ++ show x2)
{- 
-- inverzna (\x->2*x+3) 17    .. 2*7+3=17
-- inverzna (\x->x*x*x) 125   .. 5*5*5 = 125
4.999992
-- inverzna (\x->x*x*x+3*x+1) 100
4.4100904
(\x->x*x*x+3*x+1) 4.4100904
100.0016666328384
-}

-------------------------------------------------------------------
-- kompozícia zoznamu funkcií,  :: [a->a] -> (a->a)

-- zaciatocnicka definicia cez zoznamovu rekurziu
kompozicia  :: [a->a] -> (a->a)
kompozicia [] = id
kompozicia (f:fs) = (\x -> f (kompozicia fs x))

-- definicia haskellistu, co si nasiel operator $
kompozicia''  :: [a->a] -> (a->a)
kompozicia'' [] = id
kompozicia'' (f:fs) = \x -> f $ kompozicia'' fs x

-- definicia haskellistu, co si este prehodil x na lavu stranu
kompozicia'''''  :: [a->a] -> (a->a)
kompozicia''''' [] x      = x
kompozicia''''' (f:fs) x  = f $ kompozicia''''' fs x

-- jemne pokrocily haskellista, ktory bol na prednaske
kompozicia'  :: [a->a] -> (a->a)
kompozicia' [] = id
kompozicia' (f:fs) = f . kompozicia' fs

-- haskellista, co si pamata, ze skladanie funkcii je asociativne ale nepamata, ze nie je komutativne
kompozicia''''  :: [a->a] -> (a->a)
kompozicia'''' [] = id
kompozicia'''' (f:fs) = kompozicia'''' fs . f

-- haskellista, co bude volit lavicu
kompoziciaLeft  :: [a->a] -> (a->a)
kompoziciaLeft = foldl (.) id

-- haskellista, co bude volit neexistujucu pravicu
kompoziciaRight  :: [a->a] -> (a->a)
kompoziciaRight = foldr (.) id

zoznamfcii = [(+7),(*11),(`mod` 1234567),(`div` 10),(^4),(+1),(*2),(^3)]

{-
*Main> kompozicia      zoznamfcii 1
95
*Main> kompozicia''    zoznamfcii 1
95
*Main> kompozicia''''' zoznamfcii 1
95
*Main> kompozicia'     zoznamfcii 1
95
*Main> kompozicia''''  zoznamfcii 1
550158565384
*Main> kompoziciaLeft  zoznamfcii 1
95
*Main> kompoziciaRight zoznamfcii 1
95

-- kompozicia funkcii nie je komutativna

*Main> kompozicia      (reverse zoznamfcii) 1
550158565384
*Main> kompozicia''    (reverse zoznamfcii) 1
550158565384
*Main> kompozicia''''' (reverse zoznamfcii) 1
550158565384
*Main> kompozicia'     (reverse zoznamfcii) 1
550158565384
*Main> kompozicia''''  (reverse zoznamfcii) 1
95
*Main> kompoziciaLeft  (reverse zoznamfcii) 1
550158565384
*Main> kompoziciaRight (reverse zoznamfcii) 1
550158565384

-- evidentne definicia kompozicia'''' je zla, kedze predpokladala komutativnost (.)
-}
