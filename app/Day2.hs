{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Parser

data Line = Line
  { mini :: Int,
    maxi :: Int,
    ch :: Char,
    password :: String
  }
  deriving (Show)

parseLine :: Parser Line
parseLine = do
  (min', max') <- (,) <$> natural <* char '-' <*> natural
  whiteSpace
  (c, str) <- (,) <$> anyChar <* string ": " <*> stringLiteral
  return $ Line min' max' c str

checkPassword1 :: Line -> Bool
checkPassword1 l = inRange c (mini l) (maxi l)
  where
    c = count (ch l) $ password l

solve1 :: [String] -> Int
solve1 = length . filter (checkPassword1 . unsafeParse parseLine)

checkPassword2 :: Line -> Bool
checkPassword2 l = (f == c) /= (a == c)
  where
    f = (password l) !! ((mini l) - 1)
    a = (password l) !! ((maxi l) - 1)
    c = ch l

solve2 :: [String] -> Int
solve2 = length . filter (checkPassword2 . unsafeParse parseLine)

main :: IO ()
main = mainWrapper "day2" solve1 solve2
