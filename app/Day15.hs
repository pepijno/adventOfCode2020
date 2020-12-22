module Main where

import qualified Data.Map.Strict as M
import Lib

nextNumber :: ((Int, Int), M.Map Int Int) -> ((Int, Int), M.Map Int Int)
nextNumber ((a, b), m)
  | not $ M.member a m = ((0, b + 1), M.insert a b m)
  | otherwise = ((b - lastTurn, b + 1), M.insert a b m)
  where
    lastTurn = M.findWithDefault 0 a m

solve1 :: [String] -> Int
solve1 xs = fst . fst . (!! max 0 (2020 - length pairs)) $ iterate nextNumber (last pairs, M.fromList $ init pairs)
  where
    pairs = zip (map read xs) [0 ..]

solve2 :: [String] -> Int
solve2 xs = fst . fst . (!! max 0 (30000000 - length pairs)) $ iterate nextNumber (last pairs, M.fromList $ init pairs)
  where
    pairs = zip (map read xs) [0 ..]

main :: IO ()
main = mainWrapper "day15" solve1 solve2
