module SudokuStvorce where

sudokuStvorce :: [[Int]] -> [[Int]]
sudokuStvorce m    = concat [ [ [ m!!(3*i+k)!!(3*j+l) | k <- [0..2], l <- [0..2] ] | j <- [0..2] ] | i <- [0..2]]
