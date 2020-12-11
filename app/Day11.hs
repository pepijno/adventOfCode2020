{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lib
import Data.Maybe
import qualified Data.Map.Strict as M
import GHC.List (iterate')

type Coord = (Int, Int)

data Spot = Empty | Filled | Floor deriving (Show, Eq)

type Grid = M.Map Coord Spot

adjacent1 :: Grid -> Coord -> [Spot]
adjacent1 m (a, b) = mapMaybe (flip M.lookup m) [(a-1,b-1),(a-1,b),(a-1,b+1),(a,b-1),(a,b+1),(a+1,b-1),(a+1,b),(a+1,b+1)]

(!+) :: Coord -> Coord -> Coord
(!+) (a,b) (x,y) = (a+x, b+y)

n8 = [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

data Dir8 = N | NE | E | SE | S | SW | W | NW deriving (Show, Eq, Ord, Bounded, Enum)

enumerate :: forall a. (Bounded a, Enum a) => [a]
enumerate = enumFrom (minBound @a)

d2p8 :: Dir8 -> Coord
d2p8 d = M.fromList (zip enumerate n8) M.! d

step8 :: Coord -> Dir8 -> Coord
step8 = step d2p8

step :: (a -> Coord) -> Coord -> a -> Coord
step d2p p d = p !+ d2p d

walk :: Grid -> Coord -> Dir8 -> Maybe Spot
walk m pos dir = case M.lookup (step8 pos dir) m of
                   Nothing -> Nothing
                   Just c | c /= Floor -> Just c
                   _ -> walk m (step8 pos dir) dir

adjacent2 :: Grid -> Coord -> [Spot]
adjacent2 m pos = mapMaybe (walk m pos) (enumerate @Dir8)

parseSpot :: Char -> Spot
parseSpot '#' = Filled
parseSpot 'L' = Empty
parseSpot '.' = Floor

applyRule :: Int -> (Grid -> Coord -> [Spot]) -> Grid -> Coord -> Spot
applyRule thresh adj m pos
  | c == Empty = if not $ Filled `elem` adj' then Filled else Empty
  | c == Filled = if (length $ filter (==Filled) adj') >= thresh then Empty else Filled
  | otherwise = Floor
  where adj' = adj m pos
        c = M.findWithDefault Floor pos m

doStep :: (Grid -> Coord -> Spot) -> (Grid, Bool) -> (Grid, Bool)
doStep rule (m, _) = (m', m == m')
  where m' = M.foldrWithKey' (foldF m) M.empty m
        foldF mm k _ a = M.insert k (rule mm k) a

solve :: (Grid -> Coord -> Spot) -> Grid -> Grid
solve rule m = fst . head . dropWhile ((==False) . snd) . iterate' (doStep rule) $ (m, False)

solve1 :: [String] -> Int
solve1 xs = length . filter (==Filled) . M.elems $ solve (applyRule 4 adjacent1) m
  where ls = map (map parseSpot) xs
        m = M.fromList [((row, col), b) | (row, line) <- zip [0 ..] ls, (col, b) <- zip [0 ..] line]

solve2 :: [String] -> Int
solve2 xs = length . filter (==Filled) . M.elems $ solve (applyRule 5 adjacent2) m
  where ls = map (map parseSpot) xs
        m = M.fromList [((row, col), b) | (row, line) <- zip [0 ..] ls, (col, b) <- zip [0 ..] line]

main :: IO()
main = mainWrapper "day11" solve1 solve2
