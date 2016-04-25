-- nieco o efektivnosti
-- podla R. Bird: Thinking Functionaly with Haskell, 7.kapitola

{- 
lenive vyhodnocovanie
argumenty sa vyhodnocuju az ked potrebna ich hodnota
ale to nie je celkom pravda :-)
-}

sqr x = x * x
{- 
sqr(sqr(3+4))
2401
(0.02 secs, 2,923,652 bytes)
Naivne by to mohlo znamenat, ze
ked zavolate funkciu, vyhodnocuje sa az ked to treba. 
Kedze chcete vypisat hodnotu vysledku, treba vyhodnotit argument. 
Ako prve sa do pravej strany funkcie sqr dosadi argument (za x) a dostaneme
sqr(3+4)*sqr(3+4) =
((3+4)*(3+4))*((3+4)*(3+4))=
2401
to by vysvetlovalo pouzitu pamat..., ale to nebolo lenive vyhodnocovanie.
Skuste
let x = sqr(5+4) in x*x
6561
(0.00 secs, 0 bytes)
rozdiel, ze?
sqr(5+4) sa vyhodnoti len raz. 
Vypocitana hodnota je v x, takze x*x sa moze okamzite vypocitat.
Zdalo by sa, ze lenive v. je 
ked sa argumenty vyhodnotia az ked ich treba a vzdy len raz.
Ale ani to nie je celkom pravda.
Keby bol jeden z argumentov zoznam, mozno ho nemusiem vyhodnotit cely...
napr
sqr (head xs) a xs = [4,1,3,2]
uz viete, ze staci vyhodnocovat do WHNF
vyraz je v WHNF ked je to 
1. funkcie (ako napr. sqr) alebo
2. zacina data konstruktorom (ako napr. (:))
Lenive v. je ked treba len raz a do WHNF
-}

-- doplnte trivialny pripad 
subseqs1 [] = [[]]
subseqs1 (x:xs) = subseqs1 xs ++ map (x:) (subseqs1 xs)
subseqs2 []= [[]]
subseqs2 (x:xs) = xss ++ map (x:) xss
  where xss = subseqs2 xs

{-
subseqs1 [1..10]
...vypis vsetkych popdpostupnosti
(0.11 secs, 4,426,248 bytes)
subseqs2 [1..10]
...vypis vsetkych popdpostupnosti
(0.09 secs, 7,139,720 bytes)
druhy sposob by mal byt rychlejsi, kvoli where - podpostupnosti sa pocitali iba raz,
ale nie je...
ale ked odstaranime vypis vsetkych popdpostupnosti
*Main> length (subseqs1 [1..15])
32768
(0.25 secs, 26,150,896 bytes)
*Main> length (subseqs2 [1..15])
32768
(0.02 secs, 0 bytes)
*Main> length (subseqs1 [1..15])
32768
(0.19 secs, 20,155,316 bytes)
*Main> length (subseqs2 [1..15])
32768
(0.02 secs, 0 bytes)
*Main> length (subseqs2 [1..20])
1048576
(0.56 secs, 75,384,892 bytes)
*Main> length (subseqs1 [1..20])
1048576
(6.44 secs, 894,305,692 bytes)
je to podla ocakavania
-}  

foo1 n = sum (take n primes)
  where
    primes = [x|x<-[2..], divisors x == [x]]
    divisors x = [d|d<-[2..x], x `mod` d == 0]
    
foo2 n = sum (take n primes)
primes = [x|x<-[2..], divisors x == [x]]
divisors x = [d|d<-[2..x], x `mod` d == 0]

{-
*Main> foo1 1000
3682913
(5.87 secs, 519,229,628 bytes)
*Main> foo1 1000
3682913
(10.28 secs, 521,739,796 bytes)
*Main> foo2 1000
3682913
(13.17 secs, 522,982,652 bytes)
*Main> foo2 1000
3682913
(0.00 secs, 0 bytes)
to comu nerozumiem, je 2x vacsi cas pri druhom volani foo1
-}

---- priestor
myfoldl :: (b->a->b)->b->[a]->b
myfoldl f vys [] = vys
myfoldl f vys (x:xs) = myfoldl f (f vys x) xs
mojsucet = myfoldl (+) 0
-- tomuto nerozumiem :-(, vyzera to tak, ze (+) sa vyhodnocuje nie lazy

sucet :: [Integer] -> Integer
sucet = foldl (+) 0
{-
*Main> sucet [1..1000]
500500
(0.00 secs, 0 bytes)
ako to?
ved by sa to malo vyhodnocovat nejako takto:
foldl (+) 0 [1..1000]=
foldl (+) (0+1) [2..1000]=
foldl (+) ((0+1)+2) [3..1000]=
...
foldl (+) (...((0+1)+2)+...+1000) []=
(...((0+1)+2)+...+1000)=
...
500500, co zaberie aspon 1000 krabic v pamati na reprezentaciu vyrazu, 
ktory sa ma vyhodnotit

ale vyhodnocuje sa to nejako takto
foldl (+) 0 [1..1000]=
foldl (+) (0+1) [2..1000]=
foldl (+) 1 [2..1000]=
foldl (+) (1+2) [3..1000]=
foldl (+) 3 [2..1000]=
...
foldl (+) 500500 []=
500500, co v pamati zaberie jedinu krabicu

bolo by uzitocne mat nastroj na kontrolu vyhodnocovania...

specialna konstrukcia seq :: a->b->b, neda sa definovat v Haskelli

x `seq` y najprv sa vyhodnoti x (do HNF),
a potom aj s vysledkom x sa vyhodnoti y
-}
mojfoldl1 :: (b->a->b)->b->[a]->b
mojfoldl1 f vys [] = vys
mojfoldl1 f vys (x:xs) = mojfoldl1 f (f vys x) xs

mojfoldl2 :: (b->a->b)->b->[a]->b
mojfoldl2 f vys [] = vys
mojfoldl2 f vys (x:xs) = y `seq` mojfoldl2 f y xs
  where y = f vys x

-- plati mojfoldl2 f vys xs == mojfoldl1 f vys xs ?
-- najdite f vys xs take, ze neplati rovnost - rozcvicka
-- riesenie napriklad:
f1 n x = if x == 0 then undefined else 0

g1 xs = foldl f1 0 xs
g2 xs = mojfoldl2 f1 0 xs
{-
*Main> g1 [0,2]
0
(0.00 secs, 0 bytes)
*Main> g2 [0,2]
*** Exception: Prelude.undefined
-}

--mean1 by vraj mal byt jednoprechodovy podla R. Birda
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

mean1 [] = 0
mean1 xs = s / fromIntegral n
  where (s, n) = sumlen xs

sumlen = mojfoldl2 f (0,0)
  where f (s, n) x = s `seq` n `seq` (s+x, n+1)
  
{-
*Main> mean [1..100000]
50000.5
(0.08 secs, 18,253,140 bytes)
*Main> mean1 [1..100000]
50000.5
(0.33 secs, 35,608,380 bytes)
-}  
