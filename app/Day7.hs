module Main where

import Data.List
import qualified Data.Map as M
import Lib
import Parser

type Bag = (String, [(Int, String)])

parseBag :: Parser String
parseBag = do
  a <- stringLiteral
  char ' '
  b <- stringLiteral
  string " bag"
  optional $ char 's'
  return $ a ++ b

parseChild :: Parser (Int, String)
parseChild = do
  n <- integer
  char ' '
  bag <- parseBag
  return (n, bag)

parseInput :: Parser Bag
parseInput = do
  bag <- parseBag
  string " contain "
  children <- (string "no other bags" >> pure []) <| sepBy (string ", ") parseChild
  char '.'
  return (bag, children)

parseAll :: [String] -> [Bag]
parseAll = map (unsafeParse parseInput)

countContained :: [Bag] -> Int
countContained bs = length (converge containedByAny ["shinygold"]) - 1
  where
    containedByAny bs' = nub $ sort $ bs' ++ map fst (filter (matchAny bs') bs)
    matchAny xs (_, ys) = not $ null [undefined | x <- xs, (_, y) <- ys, x == y]

solve1 :: [String] -> Int
solve1 = countContained . parseAll

countChildren :: String -> M.Map String [(Int, String)] -> Int
countChildren key m
  | null children = 0
  | otherwise = sum $ map countChild children
  where
    children = M.findWithDefault [] key m
    countChild (x, y) = x * (1 + countChildren y m)

solve2 :: [String] -> Int
solve2 = countChildren "shinygold" . M.fromList . parseAll

main :: IO ()
main = mainWrapper "day7" solve1 solve2
