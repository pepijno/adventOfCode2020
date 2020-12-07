{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

data Line = Line {
    mini :: Int,
    maxi :: Int,
    ch :: Char,
    password :: String
} deriving (Show)

parseLine :: Parser Line
parseLine = Line <$> integer <* char '-' <*> integer <* whiteSpace <*> anyChar <* string ": " <*> stringLiteral

checkPassword1 :: Line -> Bool
checkPassword1 l = inRange c (mini l) (maxi l)
  where
    c = length . filter (== (ch l)) $ password l

solve1 :: [String] -> Int
solve1 = sum . map (fromEnum . checkPassword1 . unsafeParse parseLine)

checkPassword2 :: Line -> Bool
checkPassword2 l = (f == c) /= (a == c)
  where
    f = (password l) !! ((mini l) - 1)
    a = (password l) !! ((maxi l) - 1)
    c = ch l

solve2 :: [String] -> Int
solve2 = sum . map (fromEnum . checkPassword2 . unsafeParse parseLine)

main :: IO ()
main = mainWrapper "day2" [solve1, solve2]
