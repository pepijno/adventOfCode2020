module Main where

import Data.Bits
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Lib
import Parser

data Instruction = Mask (Integer, Integer, S.Set Int) | WriteMem Integer Integer deriving (Show)

parseWrite :: Parser Instruction
parseWrite = do
  string "mem["
  addr <- fromIntegral <$> natural
  string "] = "
  val <- fromIntegral <$> natural
  return $ WriteMem addr val

parseMask :: Parser Instruction
parseMask = string "mask = " *> (stringToMask <$> stringLiteral)

parseInstruction :: Parser Instruction
parseInstruction = parseWrite <|> parseMask

parseBinary :: (Integer, Integer, S.Set Int) -> Int -> String -> (Integer, Integer, S.Set Int)
parseBinary l _ [] = l
parseBinary (a, b, s) n (x : xs) = case x of
  '1' -> parseBinary (2 * a + 1, 2 * b, s) (n - 1) xs
  '0' -> parseBinary (2 * a, 2 * b + 1, s) (n - 1) xs
  'X' -> parseBinary (2 * a, 2 * b, S.insert n s) (n - 1) xs
  _ -> error "unknown parse"

stringToMask :: String -> Instruction
stringToMask = Mask <$> parseBinary (0, 0, S.empty) 35

applyMask :: (Integer, Integer) -> Integer -> Integer
applyMask (x, y) a = (a .|. x) .&. complement y

doInstruction :: ((Integer, Integer), M.Map Integer Integer) -> Instruction -> ((Integer, Integer), M.Map Integer Integer)
doInstruction (_, m) (Mask (a, b, _)) = ((a, b), m)
doInstruction (a, m) (WriteMem mem i) = (a, M.insert mem val m)
  where
    val = applyMask a i

solve1 :: [String] -> Integer
solve1 = sum . map snd . M.toList . snd . foldl doInstruction ((0, 0), M.empty) . map (unsafeParse parseInstruction)

doInstruction2 :: ((Integer, Integer, S.Set Integer), M.Map Integer Integer) -> Instruction -> ((Integer, Integer, S.Set Integer), M.Map Integer Integer)
doInstruction2 (_, m) (Mask (a, _, s)) = ((a, b', s'), m)
  where
    s' = S.map (S.foldl setBit 0) $ S.powerSet s
    b' = S.foldl setBit 0 s
doInstruction2 (a@(x, y, s), m) (WriteMem mem i) = (a, foldl (\m' addr -> M.insert addr i m') m addrs)
  where
    mem' = applyMask (x, y) mem
    addrs = map (mem' .|.) $ S.toList s

solve2 :: [String] -> Integer
solve2 = sum . map snd . M.toList . snd . foldl doInstruction2 ((0, 0, S.empty), M.empty) . map (unsafeParse parseInstruction)

main :: IO ()
main = mainWrapper "day14" solve1 solve2
