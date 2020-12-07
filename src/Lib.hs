module Lib (
  mainWrapper,
  inRange,
  groupPairs,

  Parser,
  parse,
  char,
  string,
  integer,
  stringLiteral,
  whiteSpace
) where

import Data.Char
import Data.List.Split
import Control.Applicative

mainWrapper :: (Show a) => String -> [[String] -> a] -> IO()
mainWrapper file fs = do
  contents <- readFile ("./inputs/" ++ file ++ ".txt")
  print . (<*>) fs . pure . lines $ contents

inRange :: (Ord a) => a -> a -> a -> Bool
inRange v min max = min <= v && v <= max

groupPairs :: [String] -> [[String]]
groupPairs = splitOn [""]

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    Just (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser a) <*> (Parser b) = Parser $ \input -> do
    (f, rest) <- a input
    (x, rest') <- b rest
    Just (f x, rest')

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser a) <|> (Parser b) = Parser $ \input -> (a input) <|> (b input)

char :: Char -> Parser Char
char c = Parser f
  where f (x:rest)
          | x == c = Just (x, rest)
          | otherwise = Nothing
        f [] = Nothing

string :: String -> Parser String
string = sequenceA . map char

splitBy :: (Char -> Bool) -> Parser String
splitBy f = Parser $ \input -> Just $ span f input

integer :: Parser Int
integer = read <$> splitBy isDigit

whiteSpace :: Parser String
whiteSpace = splitBy isSpace

stringLiteral :: Parser String
stringLiteral = splitBy isLetter
