module Main where

import Lib

getPath :: (Int, Int) -> Int -> [String] -> String
getPath (n,m) i [] = []
getPath (n,m) i (x:xs) = e:(getPath (n,m) (i + n) (drop (m - 1) xs))
  where p = i `mod` length x
        e = x !! p

nTrees :: (Int, Int) -> [String] -> Int
nTrees x = length . filter (=='#') . getPath x 0

solve1 :: [String] -> Int
solve1 = nTrees (3,1)

solve2 :: [String] -> Int
solve2 xs = product $ map (flip nTrees xs) [(1,1),(3,1),(5,1),(7,1),(1,2)]

main :: IO()
main = mainWrapper "day3" [solve1, solve2]

