{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Comonad (Comonad (..))
import Control.Comonad.Representable.Store (Store (..), StoreT (..), experiment, store)
import Data.Bool (bool)
import Data.Distributive (Distributive (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import Data.Monoid (Sum, getSum)
import Data.Vector (Vector, generate, (!))
import Lib

type Coord3D = (Int, (Int, Int))

type Grid3D = Store (Compose Vector (Compose Vector Vector)) Bool

type Rule3D = Grid3D -> Bool

instance Distributive Vector where
  distribute = distributeRep

instance Representable Vector where
  type Rep Vector = Int
  index v i = v ! (i `mod` gridSize)
  tabulate = generate gridSize

gridSize :: Int
gridSize = 24

neighbourCoords3D :: Coord3D -> [Coord3D]
neighbourCoords3D (a, (b, c)) = [(x, (y, z)) | x <- [-1, 0, 1], y <- [-1, 0, 1], z <- [-1, 0, 1], (x, y, z) /= (0, 0, 0)]

addCoords3D :: Coord3D -> Coord3D -> Coord3D
addCoords3D (x, (y, z)) (x', (y', z')) = (x + x', (y + y', z + z'))

basicRule3D :: Rule3D
basicRule3D g = numNeighboursAlive == 3 || (alive && numNeighboursAlive == 2)
  where
    alive = extract g
    neighbours = experiment (\s -> at3D (neighbourCoords3D s) s) g
    numNeighboursAlive = length (filter id neighbours)

step3D :: Rule3D -> Grid3D -> Grid3D
step3D = extend

render3D :: Grid3D -> Int
render3D (StoreT (Identity (Compose g)) _) = getSum $ foldMap ((+ 0) . foldMap (bool 0 1)) g

mkGrid3D :: [Coord3D] -> Grid3D
mkGrid3D xs = store (`elem` xs) (0, (0, 0))

at3D :: [Coord3D] -> Coord3D -> [Coord3D]
coords `at3D` origin = map (addCoords3D origin) coords

parseGrid3D :: [String] -> [Coord3D]
parseGrid3D xs = M.keys $ M.filter (== '#') m
  where
    m = M.fromList [((0, (col, row)), b) | (row, line) <- zip [0 ..] xs, (col, b) <- zip [0 ..] line]

type Coord4D = (Int, (Int, (Int, Int)))

type Grid4D = Store (Compose Vector (Compose Vector (Compose Vector Vector))) Bool

type Rule4D = Grid4D -> Bool

neighbourCoords4D :: Coord4D -> [Coord4D]
neighbourCoords4D (a, (b, (c, d))) = [(x, (y, (z, z'))) | z' <- [-1, 0, 1], x <- [-1, 0, 1], y <- [-1, 0, 1], z <- [-1, 0, 1], (x, y, z, z') /= (0, 0, 0, 0)]

addCoords4D :: Coord4D -> Coord4D -> Coord4D
addCoords4D (x, (y, (z, a))) (x', (y', (z', a'))) = (x + x', (y + y', (z + z', a + a')))

basicRule4D :: Rule4D
basicRule4D g = numNeighboursAlive == 3 || (alive && numNeighboursAlive == 2)
  where
    alive = extract g
    neighbours = experiment (\s -> at4D (neighbourCoords4D s) s) g
    numNeighboursAlive = length (filter id neighbours)

step4D :: Rule4D -> Grid4D -> Grid4D
step4D = extend

render4D :: Grid4D -> Int
render4D (StoreT (Identity (Compose g)) _) = getSum $ foldMap ((+ 0) . foldMap (bool 0 1)) g

mkGrid4D :: [Coord4D] -> Grid4D
mkGrid4D xs = store (`elem` xs) (0, (0, (0, 0)))

at4D :: [Coord4D] -> Coord4D -> [Coord4D]
coords `at4D` origin = map (addCoords4D origin) coords

parseGrid4D :: [String] -> [Coord4D]
parseGrid4D xs = M.keys $ M.filter (== '#') m
  where
    m = M.fromList [((0, (0, (col, row))), b) | (row, line) <- zip [0 ..] xs, (col, b) <- zip [0 ..] line]

solve1 :: [String] -> Int
solve1 = render3D . (!! 6) . iterate (step3D basicRule3D) . mkGrid3D . parseGrid3D

solve2 :: [String] -> Int
solve2 = render4D . (!! 6) . iterate (step4D basicRule4D) . mkGrid4D . parseGrid4D

main :: IO ()
main = mainWrapper "day17" solve1 solve2
