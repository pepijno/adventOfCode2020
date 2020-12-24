{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.Char
import Lib

parse :: [Int] -> (Int, UArray Int Int)
parse nums = (head nums, array (minimum nums, maximum nums) $ zip nums $ tail nums ++ [head nums])

runGame :: Int -> Int -> UArray Int Int -> UArray Int Int
runGame steps start cs = runSTUArray $ do
  m <- unsafeThaw cs
  step steps m start
  where
    step cnt m x
      | cnt == 0 = pure m
      | otherwise = do
        a <- readArray m x
        b <- readArray m a
        c <- readArray m b
        readArray m c >>= writeArray m x
        let n =
              head
                [ y' | y <- [x -1, x -2 ..], let y' = if y < 1 then y + (snd $ bounds cs) else y, y' `notElem` [a, b, c]
                ]
        readArray m n >>= writeArray m c
        writeArray m n a
        readArray m x >>= step (cnt -1) m

toSolution :: Int -> UArray Int Int -> String
toSolution xs cs
  | x == 1 = ""
  | otherwise = (intToDigit x) : toSolution x cs
  where
    x = cs ! xs

solve1 :: [String] -> Int
solve1 xs = read $ toSolution 1 $ runGame 100 start cs
  where
    xs' = map digitToInt $ head xs
    start = head xs'
    cs = array (minimum xs', maximum xs') $ zip xs' $ (tail xs' ++ [start])

solve2 :: [String] -> Int
solve2 xs = (res ! 1) * (res ! (res ! 1))
  where
    xs' = (++ [10 .. 1000000]) $ map digitToInt $ head xs
    start = head xs'
    cs = array (minimum xs', maximum xs') $ zip xs' $ (tail xs' ++ [start])
    res = runGame 10000000 start cs

main = mainWrapper "day23" solve1 solve2
