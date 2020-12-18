module Main where

import Lib
import Data.List
import Data.Maybe
import Control.Monad

preamble :: Int
preamble = 25

possibleSums :: [Int] -> [Int]
possibleSums xs = [ x + y | (x:ys) <- tails xs, y <- ys ]

findNotIsSum :: [Int] -> [Int] -> Int
findNotIsSum l@(x:xs) (y:ys) = if t y l then findNotIsSum (xs ++ [y]) ys else y
  where t y (z:zs) = y - z `elem` zs || t y zs
        t _ [] = False

solveFind :: [Int] -> Int
solveFind xs = findNotIsSum (take preamble xs) (drop preamble xs)

solve1 :: [String] -> Int
solve1 = solveFind . map read

subs :: [Int] -> [[Int]]
subs = concatMap tails . inits

solveFind' :: [Int] -> [Int]
solveFind' xs = let n = solveFind xs
                 in (head . filter ((==n) . sum) . subs) xs

solve2 :: [String] -> Int
solve2 = sum . (<*>) [minimum, maximum] . (:[]) . solveFind' . map read

main :: IO()
main = mainWrapper "day9" solve1 solve2
