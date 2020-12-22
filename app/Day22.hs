module Main where

import qualified Data.Set as S
import Lib

parseDecks :: [String] -> [[Int]]
parseDecks = map (map read . tail) . groupPairs

playGame :: ([Int], [Int]) -> [Int]
playGame ([], a) = a
playGame (a, []) = a
playGame (x : xs, y : ys)
  | x > y = playGame (xs ++ [x, y], ys)
  | otherwise = playGame (xs, ys ++ [y, x])

scoreWinner :: [Int] -> Int
scoreWinner = sum . zipWith (*) [1 ..] . reverse

solve1 :: [String] -> Int
solve1 xs = scoreWinner $ playGame (p1, p2)
  where
    [p1, p2] = parseDecks xs

playGame' :: (S.Set ([Int], [Int]), ([Int], [Int])) -> (S.Set ([Int], [Int]), ([Int], [Int]))
playGame' a@(_, ([], _)) = a
playGame' a@(_, (_, [])) = a
playGame' (s, (l@(x : xs), m@(y : ys)))
  | S.member (l, m) s = (s, (l, []))
  | x <= length xs && y <= length ys =
    let (_, (_, ys')) = playGame' (S.empty, (take x xs, take y ys))
     in if null ys'
          then playGame' (s', (xs ++ [x, y], ys))
          else playGame' (s', (xs, ys ++ [y, x]))
  | x > y = playGame' (s', (xs ++ [x, y], ys))
  | otherwise = playGame' (s', (xs, ys ++ [y, x]))
  where
    s' = S.insert (l, m) s

solve2 :: [String] -> Int
solve2 xs = scoreWinner w
  where
    [p1, p2] = parseDecks xs
    (_, (w1', w2')) = playGame' (S.empty, (p1, p2))
    w = if null w1' then w2' else w1'

main :: IO ()
main = mainWrapper "day22" solve1 solve2
