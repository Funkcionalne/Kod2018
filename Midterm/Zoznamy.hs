-- v celom tomto príklade je abeceda ['a'..'z']
-- pripomíname, že typ String = [Char], teda "ahoj" a ['a','h','o'','j'] sú identické hodnoty
-- preto s reazcom pracujeme ako so zoznamom

-- 1bod
-- vráti ¾ubovo¾ný reazec, ktorý sa nenachádza v neprázdnom vstupnom zozname neprázdnych reazcov
-- Poznámka: zoznam môže ma aj jeden prvok...
unikatny  :: [String] -> String
unikatny [x] = x ++ x
unikatny  xs = concat xs

-- 2body
-- vráti zoznam všetkých najdlhších reazov (t.j. maximálnej dåžky) zo vstupného zoznamu. Na poradí nezáleží.
najdlhsie  :: [String] -> [String]
najdlhsie xs = filter ( (== maximum (map length xs)) . length) xs


-- 3body
-- na jeden prechod zoznamu
najdlhsie'  :: [String] -> [String]
najdlhsie' xs = fst $ jedenPrechod xs (-1)
    where jedenPrechod [] max = ([], max)
          jedenPrechod (x:xs) max = (if m == max' then x:maxs else maxs, max') 
            where m = length x
                  (maxs, max') = jedenPrechod xs (if m > max then m else max)
            
-- 3body
-- vrati najkratsi neprazdny retazec, ktory sa nenachadza v zozname retazcov
alls = []:[ x:a | a<-alls,x<-['a'..'z']]
najkratsi :: [String] -> String
najkratsi xs = head (filter (`notElem` xs) (tail alls))

