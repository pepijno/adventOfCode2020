module Main where

import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as M
import Lib
import Parser

type Passport = M.Map String String

isValid :: String -> String -> Bool
isValid k v = case k of
  "byr" -> inRange (read v) 1920 2002
  "iyr" -> inRange (read v) 2010 2020
  "eyr" -> inRange (read v) 2020 2030
  "hgt" ->
    let xx = drop (length v - 2) v
        yy = read $ take (length v - 2) v
     in (xx == "cm" && inRange yy 150 193) || (xx == "in" && inRange yy 59 76)
  "hcl" -> (head v == '#') && (all (isHexDigit . toLower) $ tail v) && (length v == 7)
  "ecl" -> v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  "pid" -> (length v == 9) && (all isDigit v)
  _ -> True

parsePair :: Parser (String, String)
parsePair = (,) <$> letters <* char ':' <*> many1 anyChar

grouping :: [String] -> [[String]]
grouping = map (concat . map words) . groupPairs

parsePassport :: [String] -> Passport
parsePassport = M.fromList . map (unsafeParse parsePair)

validPassport1 :: Passport -> Bool
validPassport1 = (== 7) . M.size . M.filterWithKey (\k _ -> k /= "cid")

solve1 :: [String] -> Int
solve1 = count (validPassport1 . parsePassport) . grouping

validPassport2 :: Passport -> Bool
validPassport2 = (== 7) . M.size . M.filterWithKey (\k v -> k /= "cid" && isValid k v)

solve2 :: [String] -> Int
solve2 = count (validPassport2 . parsePassport) . grouping

main :: IO ()
main = mainWrapper "day4" solve1 solve2
