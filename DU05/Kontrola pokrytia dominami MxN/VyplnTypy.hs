module VyplnTypy where

data
  Pos = P Int Int
        deriving (Eq, Ord, Show, Read)

-- reprezentacia domina 
-- orientovaneho horizontalne resp. vertikalne,
-- ktoreho lavy horny roj je na pozicii Pos
data
  Domino = Horiz Pos | Vert  Pos 
        deriving (Eq, Show, Read)