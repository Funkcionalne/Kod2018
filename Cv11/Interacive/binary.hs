    module Rozcvicka where
     
     
     
    import System.IO
    --import Control.Exception
     
    hadaj100 :: Int -> Int -> IO ()
    hadaj100 od po = 
        if od == po then
            putStrLn("hladane cislo je : " ++ (show od))
        else
            do putStrLn ("je cislo vacsie ako " ++  (show (sumaPol)) ++ " (ano/nie)")
               ans <- getLine
               if ans == "nie" then
                   hadaj100 od ((suma) `div` 2)
               else if ans == "ano" then
                   hadaj100 (((suma) `div` 2) + 1) po
               else do
                   putStrLn ("nespravny tvar odpovede !!!")
                   hadaj100 od po
     
        where suma = od + po
              sumaPol = suma `div` 2