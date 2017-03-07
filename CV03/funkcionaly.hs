foldr' f a xs = foldl (flip f) a (reverse xs)

{--
"?: " foldr' (\x -> \y ->10*y+x) 0 [1..5]
54321
--}

foldr'' f a xs = foldl f' a (reverse xs)
  where
    f' x y = f y x

{--
"?: " foldr'' (\x -> \y ->10*y+x) 0 [1..5]
54321
--}

{--
"?: " :type map
map :: (a -> b) -> [a] -> [b]

vieme otypovat vyraz map map ?

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

MAP map
MAP :: (A -> B) -> [A] -> [B]
map :: (a -> b) -> ([a] -> [b])
preto A = (a->b), B = [a] -> [b]
MAP :: ((a->b) -> ([a] -> [b])) -> [a->b] -> [[a] -> [b]]
MAP map :: [a->b] -> [[a] -> [b]]

nech a = b = Int
MAP :: ((Int->Int) -> ([Int] -> [Int])) -> [Int->Int] -> [[Int] -> [Int]]

(map map) [(+1),(+2),(*3)]
nevypise, lebo je to zoznam funckcii

length $ (map map) [(+1),(+2),(*3)]
3

(map map) [(+1),(+2),(*3)]!!0 [1..10]

"?: " ((map map) [(+1),(+2),(*3)]!!0) [1..10]
[2,3,4,5,6,7,8,9,10,11]

"?: " ((map map) [(+1),(+2),(*3)]!!2) [1..10]
[3,6,9,12,15,18,21,24,27,30]

--}

-- foldl1 f [a1,...,an] = (... (f (f a1 a2) a3 ... an)
-- foldl1 f [a1] = a1
-- foldl1 f [] = undefined
-- priklad
-- foldl1 (/) [64,4,2,8] = 1

-- definujte foldl1 pomocou foldl
foldl1' f (x:xs)  = foldl f x xs


-- foldr1 f [a1,...,an] = (f a1 (f a2 ... (f a_n-1 an)))
-- foldr1 f [a1] = a1
-- foldr1 f [] = undefined
-- priklad
-- foldr1 (/) [8,12,24,4] = 4.0

-- definujte foldr pomocou foldr1
foldr''' f z xs  = foldr1 f (xs ++[z])

-- scanl f z [a1, ..., an] = [z, f z a1, f (f z a1) a2, ...]
-- scanl (\x -> \y -> 2*x + y) 4 [1,2,3] =  [4,9,20,43] 

-- definujte scanl pomocou foldl
scanl' f z xs = foldl (\acc -> \x -> acc ++ [(f (last acc) x)]) [z] xs

-- definujte scanl pomocou foldl efektivnejsie (napr. pomocou reverse)
scanl'' f z xs = reverse $ foldl (\acc -> \x -> (f (head acc) x):acc) [z] xs

-- definujte foldl pomocou scanl
foldl'' f z xs = last $ scanl f z xs

-- definujte scanr pomocou foldr
scanr' f z xs = foldr (\x -> \rek -> (f (head rek) x):rek) [z] xs

-- definujte foldr pomocou scanr
foldr'''' f z xs = head $ scanr f z xs

