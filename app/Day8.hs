module Main where

import Lib
import Parser
import qualified Data.Map as M
import Data.Maybe

data Instruction = Jmp Int | Acc Int | Nop Int deriving (Show, Eq)

parseInstruction :: Parser Instruction
parseInstruction = Jmp <$> match "jmp" <|> Acc <$> match "acc" <|> Nop <$> match "nop"
  where match s = string s *> whiteSpace *> integer

executeInstruction :: Instruction -> (Int, Int) -> (Int, Int)
executeInstruction i (acc, line) = case i of
                         Jmp x -> (acc, line + x)
                         Acc x -> (acc + x, line + 1)
                         Nop _ -> (acc, line + 1)

runProgram :: Int -> Int -> M.Map Int (Bool, Instruction) -> Int
runProgram line acc prog
  | run = acc
  | otherwise = runProgram newLine newAcc (M.insert line (True, i) prog)
    where (run, i) = M.findWithDefault (False, Nop 0) line prog
          (newAcc, newLine) = executeInstruction i (acc, line)

solve1 :: [String] -> Int
solve1 = runProgram 0 0 . M.fromList . zip [0..] . zip (repeat False) . map (unsafeParse parseInstruction)

runProgram2 :: Int -> Int -> M.Map Int (Bool, Instruction) -> Maybe Int
runProgram2 line acc prog
  | length prog == line = Just acc
  | run = Nothing
  | otherwise = runProgram2 newLine newAcc (M.insert line (True, i) prog)
    where (run, i) = M.findWithDefault (False, Nop 0) line prog
          (newAcc, newLine) = executeInstruction i (acc, line)

replaceAt :: a -> Int -> [a] -> [a]
replaceAt x pos xs = y ++ (x:ys)
  where (y,_:ys) = splitAt pos xs

replaceInstruction :: [Instruction] -> Int -> [Instruction]
replaceInstruction prog line = replaceAt (f i) line prog
  where i = prog !! line
        f z = case z of
                Jmp x -> Nop x
                Nop x -> Jmp x
                y -> y

replaceProg :: [Instruction] -> [[Instruction]]
replaceProg prog = map (replaceInstruction prog) $ [0..(length prog - 1)]

solve2 :: [String] -> Int
solve2 = head . catMaybes . map (runProgram2 0 0 . M.fromList . zip [0..] . zip (repeat False)) . replaceProg . map (unsafeParse parseInstruction)

main :: IO()
main = mainWrapper "day8" [solve1, solve2]
