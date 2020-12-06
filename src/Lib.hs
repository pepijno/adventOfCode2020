module Lib (
  mainWrapper,
  inRange,
  groupPairs
) where

import Data.List.Split

mainWrapper :: (Show a) => String -> [[String] -> a] -> IO()
mainWrapper file fs = do
  contents <- readFile ("./inputs/" ++ file ++ ".txt")
  print . (<*>) fs . pure . lines $ contents

inRange :: (Ord a) => a -> a -> a -> Bool
inRange v min max = min <= v && v <= max

groupPairs :: [String] -> [[String]]
groupPairs = splitOn [""]
