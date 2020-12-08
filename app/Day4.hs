module Main where

import Lib
import Data.Char
import Data.List.Split

data KeyValue = KeyValue {
  key :: String,
  value :: String
} deriving (Show, Eq)

type Passport = [KeyValue]

isValid :: KeyValue -> Bool
isValid KeyValue{key=k, value=v} = case k of
               "byr" -> inRange (read v) 1920 2002
               "iyr" -> inRange (read v) 2010 2020
               "eyr" -> inRange (read v) 2020 2030
               "hgt" -> let xx = drop (length v - 2) v
                            yy = read $ take (length v - 2) v
                        in (xx == "cm" && inRange yy 150 193) || (xx == "in" && inRange yy 59 76)
               "hcl" -> (head v == '#') && (all (isHexDigit . toLower) $ tail v) && (length v == 7)
               "ecl" -> v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
               "pid" -> (length v == 9) && (all isDigit v)
               _ -> True

parsePair :: Parser KeyValue
parsePair = KeyValue <$> (letters <* char ':') <*> stringLiteral

grouping :: [String] -> [[String]]
grouping = map (concat . map words) . groupPairs

parsePassport :: [String] -> Passport
parsePassport = map (unsafeParse parsePair)

validPassport1 :: Passport -> Bool
validPassport1 pp = (l == 8) || ((l == 7) && not ("cid" `elem` keys))
  where
    keys = map key pp
    l = length keys

solve1 :: [String] -> Int
solve1 = length . filter (validPassport1 . parsePassport) . grouping

validPassport2 :: Passport -> Bool
validPassport2 pp = validPassport1 pp && (all isValid pp)

solve2 :: [String] -> Int
solve2 = length . filter (validPassport2 . parsePassport) . grouping

main :: IO ()
main = mainWrapper "day4" [solve1, solve2]
