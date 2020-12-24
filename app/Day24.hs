module Main where

import Data.List
import qualified Data.Set as S
import Lib
import Parser

data Dir = E | NE | NW | W | SW | SE deriving (Show, Eq, Ord)

parseDir :: Parser Dir
parseDir = pure E <* string "e" <|> pure NE <* string "ne" <|> pure NW <* string "nw" <|> pure W <* string "w" <|> pure SW <* string "sw" <|> pure SE <* string "se"

parseLine :: Parser [Dir]
parseLine = many1 parseDir

parseAll :: [String] -> [[Dir]]
parseAll = map (unsafeParse parseLine)

moveDir :: (Int, Int) -> Dir -> (Int, Int)
moveDir (x, y) dir = case dir of
  E -> (x + 1, y)
  NE -> (x + 1, y + 1)
  NW -> (x, y + 1)
  W -> (x - 1, y)
  SW -> (x - 1, y - 1)
  SE -> (x, y - 1)

moveLine :: [Dir] -> (Int, Int)
moveLine = foldl moveDir (0, 0)

getFlipped :: [[Dir]] -> [(Int, Int)]
getFlipped = map head . filter (odd . length) . group . sort . map moveLine

solve1 :: [String] -> Int
solve1 = length . getFlipped . parseAll

nbs :: (Int, Int) -> [(Int, Int)]
nbs pos = map (moveDir pos) [E, NE, NW, W, SW, SE]

doFlip :: S.Set (Int, Int) -> S.Set (Int, Int)
doFlip s = S.filter toBlack $ S.fromList $ [(x, y) | x <- [- xMax .. xMax], y <- [- yMax .. yMax]]
  where
    xMax = 1 + (S.findMax $ S.map (abs . fst) s)
    yMax = 1 + (S.findMax $ S.map (abs . snd) s)
    nbs' = filter (\x -> S.member x s) . nbs
    toBlack (x, y)
      | n == 2 = True
      | n == 1 && S.member (x, y) s = True
      | otherwise = False
      where
        n = length $ nbs' (x, y)

solve2 :: [String] -> Int
solve2 = S.size . nSteps 100 doFlip . S.fromList . getFlipped . parseAll

main :: IO ()
main = mainWrapper "day24" solve1 solve2
