-- \x.x  
i = \x -> x

-- \xy.x  
k = \x -> \y -> x

-- \xyz.x z (y z)  
s = \x -> \y -> \z -> ((x z) (y z))
-------------------------

-- omega = \x.(x x)
-- prelozte do SKI

-- overte, ze (omega omega) -> cykli

-- Y = \f.\x. ((f (x x)) (f (x x)))
-- prelozte do SKI

-------------------------------------------

apply'   = s (s k)                            -- ?xy.x y 
apply''  = (s ( (s (k s)) ((s (k k) i)) )) (k i)
apply'''  = i

-------------------------------------------

-- Church's numeral  
-- zero = \fx.x
zero  = undefined

-- one \f.\x.(f x)
one = undefined

-- two   = \f -> \x -> f (f x)
two   =  undefined


-- Nech X = \x.(x K S K) = \x.(((x K) S) K)
-- potom vieme ukázať, že K = X X X = (X X) X
-- A tiež, že S = X . X X = X (X X)



-- Iná možnosť je, ak X = \x.((x S) K)
-- potom K = X (X (X X))
-- S = X (X (X (X X)))







egy   = s (s k)                            -- ?xy.x y 
egy''  = (s ( (s (k s)) ((s (k k) i)) )) (k i)
egy'''  = i

ketto = (s (s (k s) k)) i  
harom = (s (s (k s) k)) (s (s (k s) k) i)  
negy  = (s (s (k s) k)) ((s (s (k s) k)) (s (s (k s) k) i))  


