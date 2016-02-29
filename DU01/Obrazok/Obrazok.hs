module Obrazok where

type Obr = [[ Char ]]

nad :: Obr -> Obr -> Obr
nad = (++)

-- horizontálna symetria
prevratH :: Obr -> Obr
prevratH = reverse

-- vertikálna symetria
prevratV :: Obr -> Obr
-- prevratV obr = [reverse riadok | riadok <- obr]
prevratV obr = map reverse obr

-- zlepí dva obrázky horizontálne ved¾a seba, ale musia by rovnako vysoké
vedla :: Obr -> Obr -> Obr
vedla lavyObr pravyObr = [(lavyRiadok ++ pravyRiadok) | 
                          (lavyRiadok, pravyRiadok) <- zip lavyObr pravyObr]

-- prema¾uje x na . a naopak
vymenZnak :: Char -> Char
vymenZnak znak = if znak == 'x' then '.' else 'x'

-- prema¾uje v matici x na . a naopak
vymenFarby :: Obr -> Obr

-- vymenFarby obr = [[ vymenZnak znak | znak <- riadok] | riadok <- obr]
vymenFarby obr = map (map vymenZnak) obr

-- vytlac na konzolu
zobrazObr :: Obr -> IO()
zobrazObr = putStr . concat . map (++ "\n")

----------- a toto mate dodefinovat...
zlozZnaky :: Char -> Char -> Char
zlozZnaky znak1 znak2 = undefined

zlozObrazky :: Obr -> Obr -> Obr
zlozObrazky obr1 obr2 = undefined

obr1 = ["..xx", "xx..", ".x.x"]
obr2 = ["x.....","x.....","xxxxxx"]

-- zlozObrazky (prevratV obr2) (prevratH obr2) je ["xxxxxx","x....x","xxxxxx"]

ob3 = ["x..","x..","xxx"]
