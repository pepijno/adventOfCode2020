module Main where

import Lib
import Data.List

solve1 :: [String] -> Int
solve1 = sum . map (length . foldr1 union) . groupPairs

solve2 :: [String] -> Int
solve2 = sum . map (length . foldr1 intersect) . groupPairs

main :: IO ()
main = mainWrapper "day6" [solve1, solve2]
