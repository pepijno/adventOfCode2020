module Main where

import Lib
import Data.List

parseBinary :: [Char] -> Int -> String -> Int
parseBinary chars n = foldl (\x y -> x * 2 + y) 0 . map one
  where one x = if x `elem` chars then 1 else 0

parseID :: String -> Int
parseID = parseBinary "BR" 0

solve1 :: [String] -> Int
solve1 = maximum . map parseID

solve2 :: [String] -> Int
solve2 xs = head $ (\\) [min .. max] ids
  where
    ids = map parseID xs
    max = maximum ids
    min = minimum ids

main :: IO ()
main = mainWrapper "day5" solve1 solve2
