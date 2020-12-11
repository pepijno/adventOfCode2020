module Main where

import Lib
import Data.List
import Data.Maybe
import Control.Monad

preamble :: Int
preamble = 25

possibleSums :: [Int] -> [Int]
possibleSums xs = [ x + y | (x:ys) <- tails xs, y <- ys ]

findNotIsSum :: [Int] -> Maybe Int
findNotIsSum l@(x:xs)
  | length l <= preamble = Nothing
  | t `elem` (possibleSums tt) = findNotIsSum xs
  | otherwise = Just t
  where tt = take (preamble + 1) l
        t = last tt

solve1 :: [String] -> Int
solve1 = fromJust . findNotIsSum . map read

subs :: [Int] -> [[Int]]
subs = filter (not . null) . concat . map tails . inits

solve2 :: [String] -> Int
solve2 = sum . (<*>) [minimum, maximum] . init . filter ((==) 57195069 . sum) . subs . map read

main :: IO()
main = mainWrapper "day9" solve1 solve2
