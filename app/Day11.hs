{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lib
import Grid
import Data.Maybe
import qualified Data.Map.Strict as M
import GHC.List (iterate')

data Spot = Empty | Filled | Floor deriving (Show, Eq)

adjacent1 :: Grid Spot -> Coord -> [Spot]
adjacent1 m pos = mapMaybe (flip M.lookup m) $ map ((!+) pos) dir8

walk :: Grid Spot -> Coord -> Dir8 -> Maybe Spot
walk m pos dir = case M.lookup (step8 pos dir) m of
                   Nothing -> Nothing
                   Just c | c /= Floor -> Just c
                   _ -> walk m (step8 pos dir) dir

adjacent2 :: Grid Spot -> Coord -> [Spot]
adjacent2 m pos = mapMaybe (walk m pos) (enumerate @Dir8)

parseSpot :: Char -> Spot
parseSpot '#' = Filled
parseSpot 'L' = Empty
parseSpot '.' = Floor

applyRule :: Int -> (Grid Spot -> Coord -> [Spot]) -> Grid Spot -> Coord -> Spot
applyRule thresh adj m pos
  | c == Empty = if not $ Filled `elem` adj' then Filled else Empty
  | c == Filled = if (length $ filter (==Filled) adj') >= thresh then Empty else Filled
  | otherwise = Floor
  where adj' = adj m pos
        c = M.findWithDefault Floor pos m

doStep :: (Grid Spot -> Coord -> Spot) -> (Grid Spot, Bool) -> (Grid Spot, Bool)
doStep rule (m, _) = (m', m == m')
  where m' = M.foldrWithKey' (foldF m) M.empty m
        foldF mm k _ a = M.insert k (rule mm k) a

solve' :: (Grid Spot -> Coord -> Spot) -> Grid Spot -> Grid Spot
solve' rule m = fst . head . dropWhile ((==False) . snd) . iterate' (doStep rule) $ (m, False)

solve :: Int -> (Grid Spot -> Coord -> [Spot]) -> [String] -> Int
solve n adj = length . filter (==Filled) . M.elems . solve' (applyRule n adj) . parseGrid parseSpot

solve1 :: [String] -> Int
solve1 = solve 4 adjacent1

solve2 :: [String] -> Int
solve2 = solve 5 adjacent2

main :: IO()
main = mainWrapper "day11" solve1 solve2
