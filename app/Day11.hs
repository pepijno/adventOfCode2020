module Main where

import Data.Bool
import Data.Maybe
import qualified Data.Vector as V
import Lib

data Spot = Empty | Filled | Floor deriving (Show, Eq)

parseGrid :: [String] -> V.Vector (V.Vector Spot)
parseGrid = V.fromList . map (V.fromList . map (bool Floor Empty . (== 'L')))

grid8los :: (a -> Bool) -> V.Vector (V.Vector a) -> (Int, Int) -> [a]
grid8los isEmpty m (x, y) = mapMaybe (getFirst (x, y)) nbs
  where
    getFirst (x0, y0) (x, y) = do
      v <- get (x0, y0) (x, y)
      if isEmpty v then getFirst (x0 + x, y0 + y) (x, y) else return v
    get (x0, y0) (x, y) = do
      row <- m V.!? (y0 + y)
      row V.!? (x0 + x)
    nbs = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

mapOverGrid :: (a -> Bool) -> (a -> [a] -> b) -> V.Vector (V.Vector a) -> V.Vector (V.Vector b)
mapOverGrid isEmpty f m = V.imap (\y v -> V.imap (\x i -> modify i (x, y)) v) m
  where
    modify i pos = f i $ grid8los isEmpty m pos

updateSpot :: Int -> Spot -> [Spot] -> Spot
updateSpot thresh s ss = case s of
  Floor -> Floor
  Empty -> if count (== Filled) ss == 0 then Filled else Empty
  Filled -> if count (== Filled) ss >= thresh then Empty else Filled

step :: (Spot -> Bool) -> Int -> V.Vector (V.Vector Spot) -> V.Vector (V.Vector Spot)
step f n = mapOverGrid f (updateSpot n)

countGrid :: (a -> Bool) -> V.Vector (V.Vector a) -> Int
countGrid f = sum . map (count f) . map V.toList . V.toList

solve1 :: [String] -> Int
solve1 = countGrid (== Filled) . converge (step (const False) 4) . parseGrid

solve2 :: [String] -> Int
solve2 = countGrid (== Filled) . converge (step (== Floor) 5) . parseGrid

main :: IO ()
main = mainWrapper "day11" solve1 solve2
