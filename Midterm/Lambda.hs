-- identifikator premennej je String 
type Var = String

-- lambda termy
data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp  deriving(Eq)

-- 2 body
-- lambda term je uzavretý, teda neobsahuje vo¾né premenné, teda žiaden výskyt žiadnej premennej nie je vo¾ný
uzavrety  :: LExp -> Bool
uzavrety  t = preliezacka t []
    where preliezacka (ID x) variables = elem x variables
          preliezacka (APP m n) variables = preliezacka m variables && preliezacka n variables
          preliezacka (LAMBDA x m) variables = preliezacka m (x:variables)

-- 3 body          
alpha  :: LExp -> LExp -> Bool
alpha  t1 t2 = preliezacka t1 t2 []
    where preliezacka (ID x) (ID y) variables = elem (x,y) variables
          preliezacka (APP m1 n1) (APP m2 n2) variables = preliezacka m1 m2 variables && preliezacka n1 n2 variables
          preliezacka (LAMBDA x m) (LAMBDA y n) variables = preliezacka m n ((x,y):variables)
          preliezacka _ _ _ = False
