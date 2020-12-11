module Lib (
  mainWrapper,
  inRange,
  groupPairs
) where

import Data.List.Split

mainWrapper :: (Show a) => String -> ([String] -> a) -> ([String] -> a) -> IO()
mainWrapper file f1 f2 = do
  contents <- lines <$> readFile ("./inputs/" ++ file ++ ".txt")
  print $ f1 contents
  print $ f2 contents

inRange :: (Ord a) => a -> a -> a -> Bool
inRange v min max = min <= v && v <= max

groupPairs :: [String] -> [[String]]
groupPairs = splitOn [""]
