module Main where

import Data.Either
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V
import Lib
import Parser

data Instruction = Jmp Int | Acc Int | Nop Int deriving (Show, Eq)

parseInstruction :: Parser Instruction
parseInstruction = Jmp <$> match "jmp" <|> Acc <$> match "acc" <|> Nop <$> match "nop"
  where
    match s = string s *> whiteSpace *> integer

parseAll :: [String] -> V.Vector Instruction
parseAll = V.fromList . map (unsafeParse parseInstruction)

executeInstruction :: Instruction -> (Int, Int) -> (Int, Int)
executeInstruction i (acc, line) = case i of
  Jmp x -> (acc, line + x)
  Acc x -> (acc + x, succ line)
  Nop _ -> (acc, succ line)

runProgram :: S.Set Int -> Int -> Int -> V.Vector Instruction -> Either Int Int
runProgram s acc line v
  | S.member line s = Left acc
  | otherwise = case v V.!? line of
    Nothing -> Right acc
    Just x ->
      let (acc', line') = executeInstruction x (acc, line)
       in runProgram s' acc' line' v
      where
        s' = S.insert line s

startProgram :: V.Vector Instruction -> Either Int Int
startProgram = runProgram S.empty 0 0

solve1 :: [String] -> Int
solve1 = fromLeft 0 . startProgram . parseAll

changeInstruction :: Instruction -> Instruction
changeInstruction (Jmp x) = Nop x
changeInstruction (Nop x) = Jmp x
changeInstruction x = x

replaceInVector :: Int -> (a -> a) -> V.Vector a -> V.Vector a
replaceInVector index f v = v V.// [(index, f $ v V.! index)]

solve2 :: [String] -> Int
solve2 xs = head $ do
  i <- [0 ..]
  Right a <- return $ startProgram $ replaceInVector i changeInstruction v
  return a
  where
    v = parseAll xs

main :: IO ()
main = mainWrapper "day8" solve1 solve2
