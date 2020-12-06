module Main where

import Lib

solve1 :: [String] -> Int
solve1 xs = head $ [x * y | x <- ys, y <- ys, x + y == 2020]
  where
    ys = map read xs

solve2 :: [String] -> Int
solve2 xs = head $ [x * y * z | x <- ys, y <- ys, z <- ys, x + y + z == 2020]
  where
    ys = map read xs

main :: IO ()
main = mainWrapper "day1" [solve1, solve2]
