module Day10 (first, second) where

import Algorithm.Search (bfs)
import Data.List (partition)
import qualified Data.Matrix as MX
import Data.Maybe (catMaybes)

type Coordainte = (Int, Int)

parse :: String -> MX.Matrix Int
parse text = MX.mapPos (\_ v -> read [v]) $ MX.fromLists $ lines text

neighbors :: Coordainte -> [Coordainte]
neighbors (y, x) = [(y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)]

moves :: (Eq a, Enum a) => MX.Matrix a -> Coordainte -> [Coordainte]
moves mtx (y, x) = filter (\(y, x) -> MX.safeGet y x mtx == Just (succ current)) $ neighbors (y, x)
  where
    current = MX.getElem y x mtx

findPath :: MX.Matrix Int -> Coordainte -> Coordainte -> Maybe [Coordainte]
findPath mtx start end = bfs (moves mtx) (== end) start

findPaths :: MX.Matrix Int -> Coordainte -> Coordainte -> Int
findPaths mtx start end = length left + rest
  where
    (left, right) = partition (== end) (moves mtx start)
    rest = foldl (\acc p -> acc + findPaths mtx p end) 0 right

findCoordinates :: MX.Matrix b -> (b -> Bool) -> [Coordainte]
findCoordinates mtx predicate = fst <$> filter (predicate . snd) (MX.toList $ MX.mapPos (,) mtx)

first :: String -> String
first input = show $ length $ catMaybes paths
  where
    mtx = parse input
    trailheads = findCoordinates mtx (== 0)
    ends = findCoordinates mtx (== 9)
    candidates = (,) <$> trailheads <*> ends
    paths = uncurry (findPath mtx) <$> candidates

second :: String -> String
second input = show $ sum paths
  where
    mtx = parse input
    trailheads = findCoordinates mtx (== 0)
    ends = findCoordinates mtx (== 9)
    candidates = (,) <$> trailheads <*> ends
    paths = uncurry (findPaths mtx) <$> candidates