module Main where

import Lib
import Data.List.Split

data Line = Line {
    mini :: Int,
    maxi :: Int,
    char :: Char,
    password :: String
} deriving (Show)

parseLine :: String -> Line
parseLine l = Line { mini=minis, maxi=maxis, char=ch, password=passw }
  where w = words l
        ch = head . head . tail $ w
        mm = splitOn "-" $ head w
        minis = (read $ head mm) :: Int
        maxis = (read $ last mm) :: Int
        passw = last w

checkPassword1 :: Line -> Bool
checkPassword1 l = inRange c (mini l) (maxi l)
  where
    c = length . filter (== (char l)) $ password l

solve1 :: [String] -> Int
solve1 = sum . map (fromEnum . checkPassword1 . parseLine)

checkPassword2 :: Line -> Bool
checkPassword2 l = (f == c) /= (a == c)
  where
    f = (password l) !! ((mini l) - 1)
    a = (password l) !! ((maxi l) - 1)
    c = char l

solve2 :: [String] -> Int
solve2 = sum . map (fromEnum . checkPassword2 . parseLine)

main :: IO ()
main = mainWrapper "day2" [solve1, solve2]
