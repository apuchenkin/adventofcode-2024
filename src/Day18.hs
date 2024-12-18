module Day18 (first, second) where

import Algorithm.Search (bfs, pruning)
import Data.List.Split (splitOn)
import qualified Data.Matrix as MX
import Data.Maybe (isNothing)
import Debug.Trace (traceShowId)

type Coordinate = (Int, Int)

type Bound = (Int, Int)

neighbors :: Coordinate -> [Coordinate]
neighbors (y, x) = [(y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)]

isValid :: MX.Matrix Char -> Coordinate -> Bool
isValid mtx (y, x) = case MX.safeGet y x mtx of
  Just '.' -> True
  _ -> False

fromCoordinates :: Bound -> [Coordinate] -> MX.Matrix Char
fromCoordinates (xlim, ylim) = foldl (\mtx (x, y) -> MX.setElem '#' (succ y, succ x) mtx) init
  where
    init = '.' <$ MX.zero ylim xlim

parse :: String -> [(Int, Int)]
parse text = readCoordinate <$> lines text
  where
    readCoordinate string = (read left, read right)
      where
        [left, right] = splitOn "," string

first :: String -> String
first input = show $ maybe 0 length path
  where
    coordinates = parse input
    bound = let s = 71 in (s, s)
    mtx = fromCoordinates bound $ take 1024 coordinates
    path = bfs (neighbors `pruning` (not . isValid mtx)) (== bound) (1, 1)

second :: String -> String
second input = show $ coordinates !! pred idx
  where
    coordinates = parse input
    bound = let s = 71 in (s, s)
    idx =
      until
        (\cs -> let mtx = fromCoordinates bound $ take cs coordinates in isNothing $ bfs (neighbors `pruning` (not . isValid mtx)) (== bound) (1, 1))
        (traceShowId . succ)
        1024