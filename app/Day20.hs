{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Lib

rot90 :: [[a]] -> [[a]]
rot90 = map reverse . transpose

allOrientations :: [[a]] -> [[[a]]]
allOrientations x = rots ++ map transpose rots
  where
    rots = [rot90 x, rot90 $ rot90 x, rot90 $ rot90 $ rot90 x, x]

allOrientations' :: [[[a]] -> [[a]]]
allOrientations' = rots ++ map (transpose .) rots
  where
    rots = [rot90, rot90 . rot90, rot90 . rot90 . rot90, id]

isNeighbour x y = not $ null $ intersect ys xs
  where
    xs = map head $ allOrientations x
    ys = map head $ allOrientations y

findNeighbours :: Int -> M.Map Int [String] -> [Int]
findNeighbours i m = M.keys $ M.filter (isNeighbour (m M.! i)) m'
  where
    m' = M.delete i m

tileRuleToId :: String -> Int
tileRuleToId = read . init . drop 5

findAllNeighbours :: [Int] -> M.Map Int [String] -> [(Int, [Int])]
findAllNeighbours [] _ = []
findAllNeighbours (i : is) m = (i, findNeighbours i m) : findAllNeighbours is m'
  where
    m' = M.delete i m

findShortest :: [[Int]] -> Int
findShortest xs
  | null xs' = length xs
  | length xs == 1 && length (head xs) > 1 = 1
  | otherwise = fst $ head xs'
  where
    xs' = filter ((>) 12 . length . snd) $ sortBy (compare `on` length . snd) $ zip [0 ..] xs

updateNeighbours :: [Int] -> M.Map Int [Int] -> M.Map Int [Int]
updateNeighbours [] m = m
updateNeighbours (i : is) m = updateNeighbours is (foldl (\m x -> M.insert x (nub $ (i :) $ m M.! x) m) m (m M.! i))

addToMatrix :: Int -> Int -> [[Int]] -> [[Int]]
addToMatrix y i xs
  | y == length xs = xs ++ [[i]]
  | otherwise = take y xs ++ [b ++ [i]] ++ drop (y + 1) xs
  where
    b = xs !! y

removeFromMap :: Int -> M.Map Int [Int] -> M.Map Int [Int]
removeFromMap i m = M.fromList $ map (\(a, b) -> (a, (\\) b [i])) $ M.toList m

constructImage :: [[Int]] -> M.Map Int [Int] -> ([[Int]], M.Map Int [Int])
constructImage xs m
  | y == 0 || x == 0 = (addToMatrix y (head $ m M.! left) xs, removeFromMap (head $ m M.! left) m)
  | otherwise = (addToMatrix y both xs, removeFromMap both m)
  where
    y = findShortest xs
    x = if length xs == y then 0 else length (xs !! y)
    left = if x /= 0 then (xs !! y) !! (x - 1) else head (xs !! (y - 1))
    up = (xs !! (y - 1)) !! x
    both = head $ intersect (m M.! left) (m M.! up)

solve1 :: [String] -> Int
solve1 xs = M.foldlWithKey (\p k _ -> k * p) 1 $ M.filter ((== 2) . length) $ updateNeighbours (M.keys sqs) $ M.fromList $ findAllNeighbours (M.keys sqs) sqs
  where
    sqs = M.fromList $ map ((tileRuleToId . head) &&& tail) $ groupPairs xs

getRightColumn :: [String] -> String
getRightColumn = head . rot90 . rot90 . rot90

transformCorrect :: [String] -> [String] -> ([String], [String])
transformCorrect left right = head $ filter (\(a, b) -> getRightColumn a == head (transpose b)) $ concatMap (\x -> map (x,) r) l
  where
    l = allOrientations left
    r = allOrientations right

transformCorrect' :: [String] -> [String] -> [String]
transformCorrect' l right = head $ filter (\x -> rc == head (transpose x)) r
  where
    r = allOrientations right
    rc = getRightColumn l

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

toImage :: [String] -> [Int] -> M.Map Int [String] -> [[String]]
toImage _ [] m = []
toImage [] (x : y : xs) m = pairToList pair ++ toImage (snd pair) xs m
  where
    pair = transformCorrect (m M.! x) (m M.! y)
toImage y (x : xs) m = p : toImage p xs m
  where
    p = transformCorrect' y (m M.! x)

transformImage :: [[String]] -> [[[String]]] -> [[[String]]]
transformImage _ [] = []
transformImage [] (x : y : xs) = map f x : map g y : transformImage (map g y) xs
  where
    (f, g) = head $ filter (\(a, b) -> last (a x') == head (b y')) $ concatMap (\x -> map (x,) fs) fs
    fs = allOrientations'
    x' = head x
    y' = head y
transformImage x (y : xs) = map f y : transformImage (map f y) xs
  where
    f = head $ filter (\a -> last x' == head (a y')) fs
    fs = allOrientations'
    x' = head x
    y' = head y

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

submatrix :: [[a]] -> [[a]]
submatrix matrix = slice 1 8 (map (slice 1 8) matrix)

concatLine :: [[String]] -> [String]
concatLine xs = map (\z -> foldl (\y x -> y ++ (x !! z)) "" xs) [0 .. (length (head xs) - 1)]

findMonsters :: [String] -> [(Int, Int)]
findMonsters xss = filter isMonster pairs
  where
    pairs = concatMap (\x -> map (x,) [1 .. (l - 2)]) [0 .. (l - 19)]
    l = length (head xss)
    isMonster (x, y) =
      ((xss !! (y - 1)) !! (x + 18)) == '#'
        && ((xss !! y) !! x) == '#'
        && ((xss !! y) !! (x + 5)) == '#'
        && ((xss !! y) !! (x + 6)) == '#'
        && ((xss !! y) !! (x + 11)) == '#'
        && ((xss !! y) !! (x + 12)) == '#'
        && ((xss !! y) !! (x + 17)) == '#'
        && ((xss !! y) !! (x + 18)) == '#'
        && ((xss !! y) !! (x + 19)) == '#'
        && ((xss !! (y + 1)) !! (x + 1)) == '#'
        && ((xss !! (y + 1)) !! (x + 4)) == '#'
        && ((xss !! (y + 1)) !! (x + 7)) == '#'
        && ((xss !! (y + 1)) !! (x + 10)) == '#'
        && ((xss !! (y + 1)) !! (x + 13)) == '#'
        && ((xss !! (y + 1)) !! (x + 16)) == '#'

solve2 :: [String] -> Int
solve2 xs = minimum $ map ((\x -> e - x * 15) . length . findMonsters) $ allOrientations bla
  where
    bla = concatMap (concatLine . map submatrix . reverse) $ transformImage [] $ map (\x -> toImage [] x sqs) $ fst $ nSteps 143 (uncurry constructImage) ([[corner]], removeFromMap corner m)
    sqs = M.fromList $ map ((tileRuleToId . head) &&& tail) $ groupPairs xs
    m = updateNeighbours (M.keys sqs) $ M.fromList $ findAllNeighbours (M.keys sqs) sqs
    corner = head $ M.keys $ M.filter ((== 2) . length) m
    e = sum $ map (count (== '#')) bla

main :: IO ()
main = mainWrapper "day20" solve1 solve2
