module VyplnTypy where

data
  Pos = P Int Int
        deriving (Eq, Ord, Show, Read)

data
  Domino = Horiz Pos | Vert  Pos 
        deriving (Eq, Show, Read)