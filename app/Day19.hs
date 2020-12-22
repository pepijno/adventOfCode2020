module Main where

import Control.Monad
import Data.Either
import qualified Data.Map.Strict as M
import Data.Maybe
import Lib
import Parser

-- data Rule = Letter Char | And Rule Rule | Or Rule Rule | See Int deriving (Show, Eq, Read, Ord)
data Rule = Rule ([Int], [Int]) | Singleton Char deriving (Show)

parseSingleton :: Parser (Int, Rule)
parseSingleton = do
  id <- natural
  string ": \""
  c <- anyChar
  char '"'
  return (id, Singleton c)

parseMultiRule :: Parser (Int, Rule)
parseMultiRule = (,) <$> natural <* string ": " <*> (Rule <$> ((,) <$> sepBy whiteSpace natural <*> (string " | " *> sepBy whiteSpace natural <|> pure [])))

parseRule :: Parser (Int, Rule)
parseRule = parseSingleton <| parseMultiRule

parseAll :: [String] -> M.Map Int Rule
parseAll = M.fromList . map (unsafeParse parseRule)

ruleToParser :: M.Map Int Rule -> Int -> Parser ()
ruleToParser m i = case m M.! i of
  Singleton c -> void $ char c
  Rule (xs, []) -> mapM_ (ruleToParser m) xs
  Rule (xs, ys) -> mapM_ (ruleToParser m) xs <|> mapM_ (ruleToParser m) ys

solve1 :: [String] -> Int
solve1 xs = count ((== 1) . count ((== "") . snd)) $ map (parse parser) lines
  where
    parser = flip ruleToParser 0 $ parseAll $ head $ groupPairs xs
    lines = last $ groupPairs xs

solve2 :: [String] -> Int
solve2 xs = length . filter ((== 1) . length . filter ((== "") . snd)) $ map (parse parser) lines
  where
    m = parseAll $ head $ groupPairs xs
    m' = M.insert 8 (Rule ([42], [42, 8])) $ M.insert 11 (Rule ([42, 31], [42, 11, 31])) m
    parser = ruleToParser m' 0
    lines = last $ groupPairs xs

main :: IO ()
main = mainWrapper "day19" solve1 solve2
