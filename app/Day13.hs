module Main where

import Lib
import Data.List
import Data.Function
import Data.List.Split

firstBus :: Int -> [Int] -> (Int, Int)
firstBus minutes ids = head . sortBy (compare `on` snd) . zip ids $ map (\x -> x - (minutes `mod` x)) ids

parseInput :: String -> [Int]
parseInput = map read . filter (/="x") . splitOn [',']

solve1 :: [String] -> Int
solve1 xs = let (bus, m) = firstBus minutes $ parseInput $ last xs
             in bus * m
            where minutes = read $ head xs

extendedEuclid :: (Integral a) => a -> a -> (a, a, a)
extendedEuclid 0 b = (b, 0, 1)
extendedEuclid a b = let (g, u, v) = extendedEuclid r a
                      in (g, v - q * u, u)
                     where (q, r) = b `quotRem` a

modInv :: (Integral a) => a -> a -> a
modInv a m = let (_, u, _) = extendedEuclid a m
              in u `mod` m

chineseRemainderStep :: (Integral a) => (a, a) -> (a, a) -> (a, a)
chineseRemainderStep (a,b) (x, y) = (r `mod` m, m)
  where m = b * y
        r = x + y * (a - x) * (modInv y b)

chineseRemainder :: (Integral a) => [(a, a)] -> (a, a)
chineseRemainder = foldr chineseRemainderStep (0, 1)

parseInput2 :: String -> [(Integer, Integer)]
parseInput2 = map (\(a, b) -> ((read b) - a, read b)) . filter ((/=) "x" . snd) . zip [0..] . splitOn [',']

solve2 :: [String] -> Integer
solve2 xs = fst $ chineseRemainder $ parseInput2 $ last xs

main :: IO()
main = mainWrapper "day13" solve1 solve2
