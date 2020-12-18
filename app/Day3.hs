module Main where

import Lib

trees :: (Int, Int) -> [[a]] -> [a]
trees (rowStep, colStep) forest = [ cycle (forest !! row) !! col | (row,col) <- zip [0,rowStep .. length forest - 1] [0,colStep..] ]

solve1 :: [String] -> Int
solve1 = count '#' . trees (1, 3)

solve2 :: [String] -> Int
solve2 xs = product $ map (count '#' . flip trees xs) [(1,1),(1,3),(1,5),(1,7),(2,1)]

main :: IO ()
main = mainWrapper "day3" solve1 solve2

