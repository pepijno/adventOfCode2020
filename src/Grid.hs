{-# LANGUAGE ScopedTypeVariables #-}

module Grid where

import Lib
import qualified Data.Map.Strict as M

type Coord = (Int, Int)
type Grid a = M.Map Coord a

data Dir4 = U | R | D | L deriving (Show, Eq, Enum, Ord, Bounded)
data Dir8 = N | NE | E | SE | S | SW | W | NW deriving (Show, Eq, Enum, Ord, Bounded)

(!+) :: Coord -> Coord -> Coord
(!+) (a,b) (x,y) = (a+x, b+y)

dir4, dir8 :: [(Int, Int)]
dir4 = [(-1, 0), (1, 0), (-1, 0), (1, 0)]
dir8 = [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

enumerate :: forall a . (Bounded a, Enum a) => [a]
enumerate = enumFrom (minBound @a)

dir4toCoord :: Dir4 -> Coord
dir4toCoord d = M.fromList (zip enumerate dir4) M.! d

dir8toCoord :: Dir8 -> Coord
dir8toCoord d = M.fromList (zip enumerate dir8) M.! d

step4 :: Coord -> Dir4 -> Coord
step4 = step dir4toCoord

step8 :: Coord -> Dir8 -> Coord
step8 = step dir8toCoord

step :: (a -> Coord) -> Coord -> a -> Coord
step dirToCoord c d = c !+ dirToCoord d

move :: (a -> Coord) -> Coord -> [a] -> Coord
move dirToCoord = foldr (flip (step dirToCoord))

parseGrid :: (Char -> a) -> [String] -> Grid a
parseGrid cellParser xs = M.map cellParser m
  where m = M.fromList [((row, col), b) | (row, line) <- zip [0 ..] xs, (col, b) <- zip [0 ..] line]
