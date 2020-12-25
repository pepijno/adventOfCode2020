module Main where

import Lib

doStep :: Int -> Int -> Int
doStep s x = (s * x) `rem` 20201227

findLoopSize :: Int -> Int
findLoopSize x = length $ takeWhile (/= x) $ iterate (doStep 7) $ 1

solve1 :: [String] -> Int
solve1 xs = nSteps c' (doStep d) $ 1
  where
    [c, d] = map read xs
    c' = findLoopSize c

solve2 :: [String] -> Int
solve2 = solve1

main :: IO ()
main = mainWrapper "day25" solve1 solve2
