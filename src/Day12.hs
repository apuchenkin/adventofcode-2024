{-# LANGUAGE TupleSections #-}

module Day12 (first, second) where

import Data.Function (on)
import Data.List (nub, sortBy)
import qualified Data.Map as M
import qualified Data.Matrix as MX
import Data.Tuple (swap)

type Coordinate = (Int, Int)

type Region = (Char, [Coordinate])

type NodeMap = M.Map Coordinate Char

data Direction = N | S | E | W
  deriving (Eq, Show, Ord)

moves :: [Direction]
moves = [N, S, E, W]

move :: Coordinate -> Direction -> Coordinate
move (y, x) d = case d of
  N -> (y - 1, x)
  E -> (y, x + 1)
  S -> (y + 1, x)
  W -> (y, x - 1)

parse :: String -> MX.Matrix Char
parse text = MX.fromLists $ lines text

toMap :: MX.Matrix Char -> NodeMap
toMap mtx = M.fromList $ MX.toList $ MX.mapPos (,) mtx

neighbors :: Coordinate -> [Coordinate]
neighbors (y, x) = [(y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)]

ff' :: (Eq a) => (a -> Bool) -> (a -> [a]) -> [a] -> [a] -> [a]
ff' predicate states visited current
  | null current = visited
  | otherwise = ff' predicate states visited' current'
  where
    candidates = nub $ concatMap states current
    visited' = current <> visited
    current' = filter (\c -> c `notElem` visited' && predicate c) candidates

ff :: (Eq a) => (a -> Bool) -> (a -> [a]) -> [a] -> [a]
ff predicate states = ff' predicate states []

findRegion :: NodeMap -> Region
findRegion nodes = (letter, region)
  where
    initial = head $ M.keys nodes
    letter = nodes M.! initial
    region = ff predicate neighbors [initial]
      where
        predicate coordinate = nodes M.!? coordinate == Just letter

regions :: NodeMap -> [Region]
regions nodes
  | null nodes = []
  | otherwise = region : regions nodes'
  where
    region = findRegion nodes
    nodes' = foldl (flip M.delete) nodes (snd region)

area :: Region -> Int
area (_, coordinates) = length coordinates

perimeter :: Region -> Int
perimeter (_, coordinates) = sum $ length . filter (`notElem` coordinates) . neighbors <$> coordinates

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy p' (x' : xs') = (x' : ys') : zs'
  where
    (ys', zs') = go p' x' xs'
    go p z (x : xs)
      | p z x = (x : ys, zs)
      | otherwise = ([], (x : ys) : zs)
      where
        (ys, zs) = go p x xs
    go _ _ [] = ([], [])

sides :: Region -> Int
sides (_, coordinates) = sum $ M.elems $ length <$> result
  where
    coordinateBounds c = (,c) <$> filter (\m -> move c m `notElem` coordinates) moves
    directionMap = M.fromListWith (++) $ (\(d, c) -> (d, [c])) <$> concatMap coordinateBounds coordinates
    result = M.mapWithKey (\d row -> groupBy (groupFn d) $ sortBy (sortFn d) row) directionMap
    sortFn d
      | d `elem` [N, S] = compare `on` id
      | otherwise = compare `on` swap
    groupFn d (y, x) (y', x')
      | d `elem` [N, S] = y == y' && (abs (x' - x) <= 1)
      | otherwise = (x == x') && (abs (y' - y) <= 1)

first :: String -> String
first input = show $ sum $ (\r -> area r * perimeter r) <$> regions nodes
  where
    nodes = toMap $ parse input

second :: String -> String
second input = show $ sum $ (\r -> area r * sides r) <$> regions nodes
  where
    nodes = toMap $ parse input