module Main where

import MinMax 
import Test.HUnit
import System.Random

t1  :: BTree Int
t1 = Node Nil 4 Nil

t2  :: BTree Int
t2 = Node Nil 5 t1

t3  :: BTree Int
t3 = Node t2 2 t1

t4  :: BTree Int
t4 = Node t3 1 t3

t5  :: BTree Int
t5 = Node t4 7 t4

s1  :: BTree String
s1 = Node Nil "janka" Nil

s2  :: BTree String
s2 = Node s1 "danka" s1

s3  :: BTree String
s3 = Node s2 "hanka" s2

s4  :: BTree String
s4 = Node s3 "manka" s3

s5  :: BTree String
s5 = Node s4 "tanka" s4

v1  :: BTree Bool
v1 = Node Nil True Nil

v2  :: BTree Bool
v2 = Node v1 False v1

main = do g <- getStdGen
          runTestTT $ TestList [ 
              TestList [ TestCase $ assertEqual ("minmax " ++ (show t)) (myminmax t) ( minmax t ) | t <- [t1,t2,t3,t4,t5] ]
              ,
              TestList [ TestCase $ assertEqual ("minmax " ++ (show t)) (myminmax t) ( minmax t ) | t <- [s1,s2,s3,s4,s5] ]
              ,
              TestList [ TestCase $ assertEqual ("minmax " ++ (show t)) (myminmax t) ( minmax t ) | t <- [v1,v2] ]
            ]
            
myminmax :: (Ord t) => (BTree t) -> (t, t)
myminmax (Node Nil v Nil)    = (v, v)
myminmax (Node Nil v right) = ( min v rmin, max v rmax)
	where 
	(rmin, rmax) = myminmax right	
myminmax (Node left v Nil) = ( min v lmin, max v lmax)
	where 
	(lmin, lmax) = myminmax left
myminmax (Node left v right) = ( min (min lmin rmin) v, max (max lmax rmax) v)
	where 
	(lmin, lmax) = myminmax left
	(rmin, rmax) = myminmax right
  
