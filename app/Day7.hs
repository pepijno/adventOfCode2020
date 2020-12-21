module Main where

import Data.List
import qualified Data.Map as M
import Lib
import Parser

type Bag = (String, [(Int, String)])

parseName :: Parser String
parseName = (++) <$> manyUntil anyChar whiteSpace <*> manyUntil anyChar whiteSpace

parseChild :: Parser (Int, String)
parseChild = do
  i <- integer
  whiteSpace
  name <- parseName
  string "bags" <| string "bag"
  string "." <| string ","
  return (i, name)

parseChildren :: Parser [(Int, String)]
parseChildren = noChildren <| sepBy (char ' ') parseChild
  where
    noChildren = pure [] <* string "no other bags."

parseInput :: Parser Bag
parseInput = do
  name <- parseName
  string "bags contain "
  children <- parseChildren
  return (name, children)

parseAll :: [String] -> [Bag]
parseAll = map (unsafeParse parseInput)

converge :: (Eq a) => (a -> a) -> a -> a
converge f x =
  let x' = f x
   in if x' == x then x else converge f x'

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
