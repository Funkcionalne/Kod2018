module Main where

import qualified Obrazok as F
import Test.HUnit
import System.Random

main = do
  g <- getStdGen
  runTestTT $ 
    TestList [ 
      TestList [ -- prva fcia
        TestCase $ assertEqual "zlozObrazky (prevratV obr2) (prevratH obr2)" 
                               ["xxxxxx","x....x","xxxxxx"]
                               ( F.zlozObrazky (prevratV obr2) (prevratH obr2) )
        ,
        TestCase $ assertEqual "vytvor" 
                               ["x..xx.xxx...","x..xx...x.xx","xxx.....x.xx",".xx..x...xxx",".xx..xxx.x..","...xxxxx.x.."]
                               ( F.vytvor )
        {-,
        let lst = map (`mod` 2000) $ take 40 (randoms g :: [Integer]) in
          TestList[
            TestCase $ assertEqual ("jCislaPoc " ++ (show x) ++ " " ++ (show (x+y))) 
                                   (jCislaPocet x (x+y)) 
                                   (F.jCislaPocet x (x+y)) | (x, y) <- urobDvojice lst]-}
       ]
     ]

-- riesenie tutora

type Obr = [[ Char ]]

nad :: Obr -> Obr -> Obr
nad = (++)

prevratH :: Obr -> Obr
prevratH = reverse

prevratV :: Obr -> Obr
prevratV obr = [reverse riadok | riadok <- obr]

vedla :: Obr -> Obr -> Obr
vedla lavyObr pravyObr = [(lavyRiadok ++ pravyRiadok) | 
                          (lavyRiadok, pravyRiadok) <- zip lavyObr pravyObr]
vymenZnak :: Char -> Char
vymenZnak znak = if znak == 'x' then '.' else 'x'

vymenFarby :: Obr -> Obr
vymenFarby obr = [[ vymenZnak znak | znak <- riadok] | riadok <- obr] 

zobrazObr :: Obr -> IO()
zobrazObr = putStr . concat . map (++ "\n")

zlozZnaky :: Char -> Char -> Char
zlozZnaky znak1 znak2 = if znak1 == '.' && znak2 =='.' then '.' else 'x'

zlozObrazky :: Obr -> Obr -> Obr
zlozObrazky obr1 obr2 = 
  [[(zlozZnaky znak1 znak2) | (znak1, znak2) <- zip riadok1 riadok2] | 
       (riadok1, riadok2) <- zip obr1 obr2]

obr1 = ["..xx", "xx..", ".x.x"]
obr2 = ["x.....","x.....","xxxxxx"]
obr3 = ["x..","x..","xxx"]
obr3inv = vymenFarby obr3
obr390  = prevratV obr3
obr390inv = vymenFarby obr390
obr3180 = prevratH obr390
obr3180inv = vymenFarby obr3180
obr3270 = prevratV obr3180
obr3270inv = vymenFarby obr3270

hornyrad = vedla (vedla (vedla obr3 obr390inv) obr3180) obr3270inv
dolnyrad = vedla (vedla (vedla obr3inv obr390) obr3180inv) obr3270
vytvor = nad hornyrad dolnyrad
