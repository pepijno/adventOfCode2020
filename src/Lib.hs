module Lib (
  mainWrapper,
  inRange,
  groupPairs,
  rotate,
  count,
  mapFst,
  mapSnd
) where

import Control.Arrow
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

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (==x)

rotate :: Int -> [a] -> [a]
rotate n xs = take len . drop (n `mod` len) . cycle $ xs
    where len = length xs

mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst f = map (f . fst &&& snd)

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f = map (fst &&& f . snd)
