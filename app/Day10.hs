module Main where

import Data.List
import Lib

sortedInput :: [String] -> [Int]
sortedInput xs = sort ys
  where
    xs' = map read xs
    ys = 0 : (+ 3) (maximum xs') : xs'

diffs :: [String] -> [Int]
diffs xs = zipWith (-) (tail sorted) sorted
  where
    sorted = sortedInput xs

countDiffs :: [String] -> Int
countDiffs xs = filteredLength (== 1) * filteredLength (== 3)
  where
    filteredLength f = count f . diffs $ xs

solve1 :: [String] -> Int
solve1 = countDiffs

dynamic :: [Int] -> [Int]
dynamic xs = 1 : map sumPrevious (tail xs)
  where
    sumPrevious x = sum $ map snd $ takeWhile ((< x) . fst) $ dropWhile ((> 3) . (x -) . fst) $ zip xs (dynamic xs)

solve2 :: [String] -> Int
solve2 = last . dynamic . sortedInput

main :: IO ()
main = mainWrapper "day10" solve1 solve2
