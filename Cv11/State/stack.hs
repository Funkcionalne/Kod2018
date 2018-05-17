module Stack where
import Control.Monad.State  

type Stack = [Int]  
  
{-  
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  
  
push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)  
-}

--     newtype State s a = State { runState :: s -> (a,s) }  
{-
    instance Monad (State s) where  
        return x = State $ \s -> (x,s)  
        (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                            (State g) = f a  
                                        in  g newState  
-}

  
pop :: State Stack Int  
pop = state(\(x:xs) -> (x,xs))  
  
push :: Int -> State Stack ()  
push a = state(\xs -> ((),a:xs))
------------------------------------------------
pushSeq :: Int -> State Stack ()
pushSeq 0   = return ()
pushSeq n   = do
                push n
                pushSeq (n-1)
                return ()

pushPop :: Int -> State Stack String
pushPop 0   = return ""
pushPop n   = do
                push n
                str <- pushPop (n-1)
                nn <- pop
                return (show nn ++ str)
-----------------------------------------------------------                
{-
"?: " evalState (pushPop 10) []
"10987654321"
"?: " execState (pushPop 10) []
[]
-}                

pushPop2 :: Int -> State Stack String
pushPop2 0   = return ""
pushPop2 n   = do
                stack <- get  -- daj mi stav .... push n 
                put (n:stack) -- updatuj stav
                str <- pushPop2 (n-1)
                (nn:stack') <- get  -- nn <- pop
                put stack'
                return (show nn ++ str)
                