{-
instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)
-}

-- Functors abstract the idea of mapping a function over each element of a structure

-- vyhodnotte
e1 = fmap (+1) Nothing
e2 = fmap (*2) (Just 3)
e3 = fmap not (Just False)
e3' = fmap not Nothing

-- moj strom
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving(Show)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- vyhodnotte  
e4 = fmap length (Leaf "abc")
e5 = fmap even (Node (Leaf 1) (Leaf 2))

inc :: Functor f => f Int -> f Int
inc = fmap (+1)

e6 = inc (Just 1)
e7 = inc [1,2,3,4,5]
e8 = inc (Node (Leaf 1) (Leaf 2))
e9 = inc (Left 5)
e10 = inc (Right 5)

-- ako je definovany Functor Either ??




























{-
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
    -}