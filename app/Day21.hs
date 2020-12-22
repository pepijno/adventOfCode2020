module Main where

import Control.Arrow
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Lib
import Parser

parseLine :: Parser [(String, [[String]])]
parseLine = do
  is <- sepBy whiteSpace stringLiteral
  algs <- between (string " (contains ") (char ')') (sepBy (string ", ") stringLiteral)
  return $ map (id &&& const [is]) algs

parseAll :: [String] -> [(String, [[String]])]
parseAll = M.toList . M.unionsWith (++) . map (M.fromList . unsafeParse parseLine)

parseIngs :: Parser [String]
parseIngs = extract <$> parseLine
  where
    extract (x : xs) = head $ snd x

deleteEl :: Eq a => a -> [a] -> [a]
deleteEl deleted xs = [x | x <- xs, x /= deleted]

getAlgs :: [(String, [String])] -> [(String, String)]
getAlgs [] = []
getAlgs [(x, xs)] = map (const x &&& id) xs
getAlgs xs = (alg, algNative) : getAlgs noFs
  where
    algNative = head $ snd fs
    alg = fst fs
    fs = head $ filter ((== 1) . length . snd) xs
    noFs = mapSnd (deleteEl algNative) $ filter ((/= 1) . length . snd) xs

deleteAll :: [String] -> [String] -> [String]
deleteAll [] ys = ys
deleteAll (x : xs) ys = deleteAll xs $ filter (/= x) ys

solve1 :: [String] -> Int
solve1 xs = length (concatMap (deleteAll algs . unsafeParse parseIngs) xs)
  where
    parsed = parseAll xs
    algs = map snd $ getAlgs $ mapSnd (foldl1 intersect) parsed

solve2 :: [String] -> String
solve2 xs = combine $ sortBy (compare `on` fst) algs
  where
    parsed = parseAll xs
    algs = getAlgs $ mapSnd (foldl1 intersect) parsed
    combine = foldl1 (\x z -> x ++ "," ++ z) . map snd

main :: IO ()
main = mainWrapper "day21" solve1 solve2
