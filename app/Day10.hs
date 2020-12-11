module Main where

import Lib
import Data.List

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail sorted) sorted
  where withBeginEnd = 0:((+3) $ maximum xs):xs
        sorted = sort withBeginEnd

countDiffs :: [Int] -> Int
countDiffs xs = (filteredLength (==1)) * (filteredLength (==3))
  where filteredLength f = length . filter f . diffs $ xs

solve1 :: [String] -> Int
solve1 = countDiffs . map read

-- Note: there are only a maximum of 4 1's in a group and no 2's
countArranges :: Int -> [Int] -> Int
countArranges x [1] = x
countArranges x [1,1] = 2 * x
countArranges x [1,1,1] = 4 * x
countArranges x [1,1,1,1] = 7 * x
countArranges x _ = x

solve2 :: [String] -> Int
solve2 = foldl countArranges 1 . group . diffs . map read

main :: IO()
main = mainWrapper "day10" solve1 solve2
