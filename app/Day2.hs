{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.List.Split
import Data.Attoparsec.Text (Parser, char, anyChar, decimal, parseOnly, space, many1, letter)
import Prelude hiding (lines)
import Data.Text (Text, pack, lines)

data Line = Line {
    mini :: Int,
    maxi :: Int,
    ch :: Char,
    password :: String
} deriving (Show)

parseLine :: Parser Line
parseLine = Line <$> decimal <*> ("-" *> decimal <* space) <*> (anyChar <* ": ") <*> (many1 letter)

unsafeParse :: Text -> Line
unsafeParse = fromRight . parseOnly parseLine
  where fromRight (Right a) = a
        fromRight b = error $ "failed to parse " ++ (show b)

checkPassword1 :: Line -> Bool
checkPassword1 l = inRange c (mini l) (maxi l)
  where
    c = length . filter (== (ch l)) $ password l

solve1 :: [String] -> Int
solve1 = sum . map (fromEnum . checkPassword1 . unsafeParse . pack)

checkPassword2 :: Line -> Bool
checkPassword2 l = (f == c) /= (a == c)
  where
    f = (password l) !! ((mini l) - 1)
    a = (password l) !! ((maxi l) - 1)
    c = ch l

solve2 :: [String] -> Int
solve2 = sum . map (fromEnum . checkPassword2 . unsafeParse . pack)

main :: IO ()
main = mainWrapper "day2" [solve1, solve2]
