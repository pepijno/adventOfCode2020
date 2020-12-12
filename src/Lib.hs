module Lib (
  mainWrapper,
  inRange,
  groupPairs,
  rotate
) where

import Data.List.Split

mainWrapper :: (Show a, Show b) => String -> ([String] -> a) -> ([String] -> b) -> IO()
mainWrapper file f1 f2 = do
  contents <- lines <$> readFile ("./inputs/" ++ file ++ ".txt")
  print $ f1 contents
  print $ f2 contents

inRange :: (Ord a) => a -> a -> a -> Bool
inRange v min max = min <= v && v <= max

groupPairs :: [String] -> [[String]]
groupPairs = splitOn [""]

rotate :: Int -> [a] -> [a]
rotate n xs = take len . drop (n `mod` len) . cycle $ xs
    where len = length xs
