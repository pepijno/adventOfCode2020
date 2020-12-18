module Main where

import Lib
import Parser
import Control.Applicative ((<|>), empty)

ops :: [Char] -> Parser Char
ops cs = char ' ' *> (foldl (\a c -> a <|> char c) empty cs) <* char ' '

expr :: [Char] -> [Char] -> Parser Int
expr cs ds = pure eval <*> (term cs ds) <*> many (pure (,) <*> (ops cs) <*> (term cs ds))

term :: [Char] -> [Char] -> Parser Int
term cs ds = pure eval <*> (factor cs ds) <*> many (pure (,) <*> (ops ds) <*> (factor cs ds))

factor :: [Char] -> [Char] -> Parser Int
factor cs ds = natural <|> (char '(' *> (expr cs ds) <* char ')')

eval :: Int -> [(Char,Int)] -> Int
eval x [] = x
eval x (('+', x'):xs) = eval (x + x') xs
eval x (('*', x'):xs) = eval (x * x') xs

solve1 :: [String] -> Int
solve1 = sum . map (unsafeParse (expr "+*" ""))

solve2 :: [String] -> Int
solve2 = sum . map (unsafeParse (expr "*" "+"))

main :: IO()
main = mainWrapper "day18" solve1 solve2
