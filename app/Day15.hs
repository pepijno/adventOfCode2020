module Main where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as V
import Lib

findNumber :: Int -> [Int] -> Int
findNumber i xs
  | i < l = xs !! i
  | otherwise = find' i
  where
    l = length xs
    find' target = runST $ do
      let target' = target - l
          y = last xs
          v0 = zip (init xs) [1 ..]
      v <- V.new i
      forM_ v0 $ uncurry $ V.write v
      stepM target' y l v
    stepM 0 y _ _ = return y
    stepM target' y l v = do
      n <- V.read v y
      let y' = if n == 0 then 0 else l - n
      V.write v y l
      stepM (target' - 1) y' (l + 1) v

solve1 :: [String] -> Int
solve1 = findNumber 2020 . map read

solve2 :: [String] -> Int
solve2 = findNumber 30000000 . map read

main :: IO ()
main = mainWrapper "day15" solve1 solve2
