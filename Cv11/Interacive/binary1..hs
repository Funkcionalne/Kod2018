    -- spocitaj cisla az po 0
    hadaj    :: IO ()
    hadaj    = do putStrLn "Mysli si cislo od 1 do 100!"
                  hadajFloor 1 100
     
    hadajFloor :: Int -> Int -> IO ()
    hadajFloor floor ceil | floor == ceil 	= putStrLn ("Je to cislo " ++ (show floor))
                          | otherwise 		= 	do  
    												putStrLn ("Je vacsie ako " ++ (show ((floor + ceil) `div` 2)) ++ "? y/n")
    												input <- getLine
    												if input == "y" then
    													hadajFloor (((floor + ceil) `div` 2)+1) ceil
    												else 
    													hadajFloor floor ((floor + ceil) `div` 2)