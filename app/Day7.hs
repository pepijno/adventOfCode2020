module Main where

import Lib
import Parser
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

parseName :: Parser String
parseName = (++) <$> stringLiteral <*> (whiteSpace *> stringLiteral)

parseChild :: Parser (Int, String)
parseChild = (,) <$>
  (integer <* whiteSpace) <*>
  (parseName <* bags <* delimiter)
    where bags = string " bags" <|> string " bag"
          delimiter = char '.' <|> char ','

parseChildren :: Parser [(Int, String)]
parseChildren = noChildren <|> ((:) <$> parseChild <*> many (whiteSpace *> parseChild) <|> pure [])
  where noChildren = pure [] <* string "no other bags."

parseInput :: Parser (String, [(Int, String)])
parseInput = (,) <$> (parseName <* sep) <*> parseChildren
  where sep = string " bags contain "

parseAll :: [String] -> [(String, [(Int, String)])]
parseAll = map fst . fromJust . sequenceA . map (parse parseInput)

createParentMap :: M.Map String [String] -> (String, [String]) -> M.Map String [String]
createParentMap m item = foldl (\n x -> M.insert x ((:) parent $ getItem n x) n) m children
  where parent = fst item
        children = snd item
        getItem n x = M.findWithDefault [] x n

countUniqueParents :: M.Map String [String] -> S.Set String -> String -> S.Set String
countUniqueParents m s key =
  foldl f (S.insert key s) (M.findWithDefault [] key m)
    where f visited next
            | S.member next visited = visited
            | otherwise = countUniqueParents m visited next

solve1 :: [String] -> Int
solve1 input = flip (-) 1 . S.size $ countUniqueParents (foldl createParentMap M.empty conv') S.empty "shinygold"
  where conv' = map (\(x, y) -> (x, map snd y)) $ parseAll input

countChildren :: String -> M.Map String [(Int, String)] -> Int
countChildren key m
  | length children == 0 = 0
  | otherwise = sum $ map countChild children
  where children = M.findWithDefault [] key m
        countChild (x, y) = x * (1 + countChildren y m)

solve2 :: [String] -> Int
solve2 = countChildren "shinygold" . M.fromList . parseAll

main :: IO()
main = mainWrapper "day7" solve1 solve2
