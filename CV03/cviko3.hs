-- dôkaz vlastností map/filter pomocou indukcie na zoznamoch
-- map/filter - definovane na prednáške
-- foldr/foldl - definovane na prednáške
-- Príklady:
-- foldl1 f [a1,...,an] = (... (f (f a1 a2) a3 ... an)
-- foldl1 f [a1] = a1
-- foldl1 f [] = undefined
-- priklad
-- foldl1 (/) [64,4,2,8] = 1

-- foldr pomocou foldl
myfoldr f a xs = foldl (flip f) a (reverse xs)
-- a este raz trocha inak
myfoldr1 f a xs = foldl f' a (reverse xs)
  where
    f' x y = f y x
mytakewhile p = foldr f []
  where f x y | p x = [x]++y
              | otherwise = []

-- rozcvicka
-- typ map map?
-- typ map :: (a->b)->[a]->[b]
-- zoberme si napriklad f::a->Bool
-- typ map f :: [a]->[Bool]
-- tak ze ked chceme typ map map, musime do typu map dosadit za funkciu unarnu funkciu.
-- typ map :: (a'->b')->[a']->[b']  -- aby sa nam nepoplietli typy, premenovali sme ich
-- takze typ map ako unarnej funkcie je (a'->b')->([a']->[b'])
-- a mozeme dosadit do typu map za a dame (a'->b') a za b dame ([a']->[b']), 
-- nezabudnite, ze po dosadeni prvy argument vo vysledom type map vynechavame
-- dostaneme map map :: [(a'->b')]->[[a']->[b']] a mozeme premenovat bez ciarok
-- filter p = concat . map f
-- where f x = ... doplnte
--     f x = if p x then [x] else []

------------------------------------
-- foldl1 f [a1,...,an] = (... (f (f a1 a2) a3 ... an)
-- foldl1 f [a1] = a1
-- foldl1 f [] = undefined
-- priklad
-- foldl1 (/) [64,4,2,8] = 1

-- definujte foldl1 pomocou foldl1
myfoldl1 f (x:xs)  = foldl f x xs


-- foldr1 f [a1,...,an] = (f a1 (f a2 ... (f a_n-1 an)))
-- foldr1 f [a1] = a1
-- foldr1 f [] = undefined
-- priklad
-- foldr1 (/) [8,12,24,4] = 4.0

-- definujte foldr pomocou foldr1
myfoldr f z xs  = foldr1 f (xs ++[z])


-- scanl f z [a1, ..., an] = [z, f z a1, f (f z a1) a2, ...]
-- scanl (\x -> \y -> 2*x + y) 4 [1,2,3] =  [4,9,20,43] 

-- definujte scanl pomocou foldl
myscanl f z xs = foldl (\acc -> \x -> acc ++ [(f (last acc) x)]) [z] xs

-- definujte scanl pomocou foldl efektivnejsie (napr. pomocou reverse)
myscanl' f z xs = reverse $ foldl (\acc -> \x -> (f (head acc) x):acc) [z] xs

-- definujte foldl pomocou scanl
myfoldl' f z xs = last $ scanl f z xs

-- definujte scanr pomocou foldr
myscanr f z xs = foldr (\x -> \rek -> (f (head rek) x):rek) [z] xs

-- definujte foldr pomocou scanr
myfoldr' f z xs = head $ scanr f z xs


------------------------- definujte foldr pomocou foldr1
 
-- scanr/scanl

-- scanl f z [a1, ..., an] = [z, f z a1, f (f z a1) a2, ...]
-- scanl (\x -> \y -> 2*x + y) 4 [1,2,3] =  [4,9,20,43] 

-- definujte scanl a scanr, pripadne aj ich verzie pre neprazdne zoznamy scanl1 a scanr1

------------------------ definujte scanl pomocou foldl

------------------------ definujte scanl pomocou foldl efektivnejsie (napr. pomocou reverse)

------------------------ definujte foldl pomocou scanl
myfoldl2 f a xs = last (scanl f a xs)

------------------------ definujte scanr pomocou foldr

------------------------ definujte foldr pomocou scanr
myfoldr2 f a xs = head (scanr f a xs)
------------------------ definujte map f pomocou foldr
------------------------ definujte concat pomocou foldr
------------------------ definujte list-comprehension [f x | x<-xs] pomocou foldr




myscanl :: (b->a->b)->b->[a]->[b]
myscanl f a [] = [a]
myscanl f a (x:xs) = a : myscanl f (f a x) xs

myscanr :: (a->b->b)->b->[a]->[b]
myscanr f a [] = [a]
myscanr f a (x:xs) = f x a0 : zvysok
  where
     zvysok@(a0:_) = myscanr f a xs
     