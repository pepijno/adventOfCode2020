module Main where

import Lib
import Grid

data Instruction = Forward Int | TurnLeft Int | TurnRight Int | MoveDir Coord deriving (Show, Eq)

data Ship = Ship Coord Dir4 deriving (Show, Eq)

parseInstruction :: String -> Instruction
parseInstruction ('N':xs) = MoveDir ((-1) * (read xs), 0)
parseInstruction ('E':xs) = MoveDir ((0, read xs))
parseInstruction ('S':xs) = MoveDir ((read xs, 0))
parseInstruction ('W':xs) = MoveDir ((0, (-1) * (read xs)))
parseInstruction ('F':xs) = Forward (read xs)
parseInstruction ('L':xs) = TurnLeft (((read xs) `div` 90) `mod` 4)
parseInstruction ('R':xs) = TurnRight (((read xs) `div` 90) `mod` 4)
parseInstruction _ = error "invalid instruction"

turnShipLeft :: Dir4 -> Int -> Dir4
turnShipLeft d i = flip (!!) i $ rotate ((length e) - (fromEnum d) - 1) $ e
  where e = reverse $ enumerate @Dir4

turnShipRight :: Dir4 -> Int -> Dir4
turnShipRight d i = flip (!!) i $ rotate (fromEnum d) $ enumerate @Dir4

forward :: Dir4 -> Int -> Coord
forward U x = ((-1) * x, 0)
forward R x = (0, x)
forward D x = (x, 0)
forward L x = (0, (-1) * x)

executeInstruction :: Ship -> Instruction -> Ship
executeInstruction (Ship p f) (TurnLeft i) = Ship p (turnShipLeft f i)
executeInstruction (Ship p f) (TurnRight i) = Ship p (turnShipRight f i)
executeInstruction (Ship p f) (Forward i) = Ship (p !+ (forward f i)) f
executeInstruction (Ship p f) (MoveDir i) = Ship (p !+ i) f

toAnswer :: Ship -> Int
toAnswer (Ship (a,b) _) = (abs a) + (abs b)

solve1 :: [String] -> Int
solve1 = toAnswer . foldl executeInstruction (Ship (0,0) R) . map parseInstruction

rotateWaypointRight :: Int -> Coord-> Coord
rotateWaypointRight i = foldr (.) id (replicate i rotate90Clockwise)

rotateWaypointLeft :: Int -> Coord -> Coord
rotateWaypointLeft i = foldr (.) id (replicate i rotate90CounterClockwise)

executeWithWaypoint :: (Ship, Coord) -> Instruction -> (Ship, Coord)
executeWithWaypoint (s, c) (TurnLeft i) = (s, rotateWaypointLeft i c)
executeWithWaypoint (s, c) (TurnRight i) = (s, rotateWaypointRight i c)
executeWithWaypoint ((Ship p _), c) (Forward i) = ((Ship ((i !* c) !+ p) R), c)
executeWithWaypoint (s, c) (MoveDir i) = (s, c !+ i)

solve2 :: [String] -> Int
solve2 = toAnswer . fst . foldl executeWithWaypoint ((Ship (0,0) R), (-1, 10)) . map parseInstruction

main :: IO()
main = mainWrapper "day12" solve1 solve2
