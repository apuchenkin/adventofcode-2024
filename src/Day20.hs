module Day20 (first, second) where

import Algorithm.Search (bfs, pruning)
import Data.Foldable (find)
import qualified Data.Matrix as MX
import Data.Maybe (fromMaybe)

type Coordinate = (Int, Int)

type Plot = MX.Matrix Char

type Path = [Coordinate]

data Direction = N | S | E | W
  deriving (Eq, Show, Ord)

mv :: Direction -> Coordinate -> Coordinate
mv d (y, x) = case d of
  N -> (y - 1, x)
  E -> (y, x + 1)
  S -> (y + 1, x)
  W -> (y, x - 1)

neighbors :: Coordinate -> [Coordinate]
neighbors c = flip mv c <$> [N, E, S, W]

parse :: String -> Plot
parse text = MX.fromLists $ lines text

isValid :: Plot -> Coordinate -> Bool
isValid mtx (y, x) = case MX.safeGet y x mtx of
  Just '.' -> True
  _ -> False

distance :: Coordinate -> Coordinate -> Int
distance (y, x) (y', x') = abs (y' - y) + abs (x' - x)

getHoles :: Path -> Int -> Coordinate -> [(Coordinate, Coordinate)]
getHoles path n c = (\(t, _) -> (c, t)) <$> pais
  where
    targets = zip (drop 100 $ drop 1 $ dropWhile (/= c) path) [2 ..]
    pais = filter (\(t, idx) -> c /= t && distance c t <= n && distance c t < idx) targets

getPath :: MX.Matrix Char -> Coordinate -> Coordinate -> [Coordinate]
getPath mtx start end = start : path
  where
    path =
      fromMaybe [] $
        bfs
          (neighbors `pruning` (not . isValid mtx))
          (== end)
          start

first :: String -> String
first input = show $ holes
  where
    mtx = parse input
    end = maybe (0, 0) fst $ find (\(_, c) -> c == 'E') $ MX.mapPos (,) mtx
    start = maybe (0, 0) fst $ find (\(_, c) -> c == 'S') $ MX.mapPos (,) mtx
    mtx' = MX.setElem '.' start $ MX.setElem '.' end mtx
    path = getPath mtx' start end
    holes = sum $ length . getHoles path 2 <$> path

second :: String -> String
second input = show $ holes
  where
    mtx = parse input
    end = maybe (0, 0) fst $ find (\(_, c) -> c == 'E') $ MX.mapPos (,) mtx
    start = maybe (0, 0) fst $ find (\(_, c) -> c == 'S') $ MX.mapPos (,) mtx
    mtx' = MX.setElem '.' start $ MX.setElem '.' end mtx
    path = getPath mtx' start end
    holes = sum $ length . getHoles path 20 <$> path