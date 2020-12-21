module Main where

import Control.Arrow
import Data.List
import Data.List.Split
import Lib
import Parser

data Condition = Condition
  { name :: String,
    min1 :: Int,
    max1 :: Int,
    min2 :: Int,
    max2 :: Int
  }
  deriving (Show, Eq)

parseCondition :: Parser Condition
parseCondition = do
  n <- manyUntil anyChar (string ": ")
  min1' <- natural
  char '-'
  max1' <- natural
  string " or "
  min2' <- natural
  char '-'
  max2' <- natural
  return $ Condition n min1' max1' min2' max2'

isValid :: Int -> Condition -> Bool
isValid i c = (inRange i (min1 c) (max1 c)) || (inRange i (min2 c) (max2 c))

sumOfNotValid :: [Condition] -> [Int] -> Int
sumOfNotValid cs = sum . filter (\x -> all (not . isValid x) cs)

parseInput :: [String] -> ([Condition], [Int], [[Int]])
parseInput xs =
  ( (map (unsafeParse parseCondition) . head) ps,
    (map read . splitOn "," . last) (ps !! 1),
    (map (map read . splitOn ",") . tail . last) ps
  )
  where
    ps = groupPairs xs

solve1 :: [String] -> Int
solve1 xs = (sum . map (sumOfNotValid cs)) ts
  where
    (cs, _, ts) = parseInput xs

allValid :: [Int] -> Condition -> Bool
allValid xs c = all (\x -> isValid x c) xs

repeatFilter :: [[Condition]] -> [(Int, Condition)] -> [(Int, Condition)]
repeatFilter is kps =
  ( ( head . map (\(a, b) -> (a, head b))
        . filter ((== 1) . length . snd)
        . zip [0 ..]
        . map (filter (not . flip elem vs))
    )
      is
  ) :
  kps
  where
    vs = map snd kps

validConditions :: [Condition] -> [Int] -> [Condition]
validConditions cs xs = filter (allValid xs) cs

isDepartureCondition :: Condition -> Bool
isDepartureCondition = isPrefixOf "departure" . name

solve2 :: [String] -> Int
solve2 xs = (product . map (\x -> my !! x) . map fst . filter (isDepartureCondition . snd) . last . take 21 . iterate (repeatFilter vcs)) []
  where
    (cs, my, ts) = parseInput xs
    ts' = (transpose . filter ((== 0) . sumOfNotValid cs)) ts
    vcs = map (validConditions cs) ts'

main :: IO ()
main = mainWrapper "day16" solve1 solve2
