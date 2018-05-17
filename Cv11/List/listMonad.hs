import Control.Monad

{-
instance Monad [] -- Defined in ‘GHC.Base’
instance Monad [] where
  return  c = [c]
  m >>= f   = [ y | x <- m, y <- f x]  
-}

cart xs ys  =   do  x <- xs  
                    y <- ys
                    return (x,y)

e1 = cart [1,2] [True, False]


listComprehension xs ys = [(x,y) | x<-xs, y<-ys ]
guardedListComprehension xs ys = [(x,y) | x<-xs, y<-ys, x<=y, x*y == 24 ]

monadComprehension xs ys = do { x<-xs; y<-ys; return (x,y) }
guardedMonadComprehension xs ys = do { x<-xs; y<-ys; guard (x<=y); guard (x*y==24); return (x,y) }

{-
listComprehension [1,2,3] ['a','b','c']
[(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]

guardedListComprehension [1..10] [1..10]
[(3,8),(4,6)]


monadComprehension [1,2,3] ['a','b','c']
[(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]

ain> guardedMonadComprehension [1..10] [1..10]
[(3,8),(4,6)]

-}

guardedComprehension :: [Int] -> [Int] -> [(Int,Int)]
guardedComprehension xs ys = do { 
          x<-xs; y<-ys; guard (x*y == 8); return (x,y) }

{-
guardedComprehension [1..10] [1..10]
[(1,8),(2,4),(4,2),(8,1)]
-}

pythagoras =   [(x, y, z) | z <- [1..],
                            x <- [1..z],
                            y <- [x..z],
                            x * x + y * y == z * z]

{- zle riesenie
pythagoras' =   do z <- [1..]
                   x <- [1..z]
                   y <- [x..z]
                   if x * x + y * y == z * z then return (x,y,z) else return ()
-}                                    
                            
pythagoras'' =   do z <- [1..]
                    x <- [1..z]
                    y <- [x..z]
                    if x * x + y * y == z * z then return "hogo-fogo" else []
                    return (x,y,z)

pythagoras''' =   do  z <- [1..]
                      x <- [1..z]
                      y <- [x..z]
                      if x * x + y * y == z * z then ["hogo-fogo"] else []
                      return (x,y,z)

pythagoras'''' =  do  z <- [1..]
                      x <- [1..z]
                      y <- [x..z]
                      if x * x + y * y == z * z then return () else []
                      return (x,y,z)

pythagoras''''' =  do  z <- [1..]
                       x <- [1..z]
                       y <- [x..z]
                       guard (x * x + y * y == z * z)
                       return (x,y,z)

spriatelene = do  z <- [1..]
                  guard(sd z /= z)
                  guard(sd(sd z) == z)
                  return (z, sd z)

sd x = sum [ d | d <-[1..x-1], x `mod` d == 0]
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
spriatelene'   = do   z<-[1..]
                      guard( (sucdel z) /= z)
                      guard ( sucdel (sucdel z) == z)
                      return (z, (sucdel z))
                       
                    
sucdel x = sum [i | i <- [1.. (x-1)], x `mod` i == 0 ]    

{-
Prvé dve priate¾ské èísla sú 220 a 284.

Vlastné delitele 220 sú: 1, 2, 4, 5,10, 11, 20, 22, 44, 55, 110 ich súèet je 284.

Vlastné delitele 284 sú 1, 2, 4, 71, 142 a ich súèet je 220.
-}                