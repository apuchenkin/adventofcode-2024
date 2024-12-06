module Day6 (first, second) where

import Data.Foldable (find)
import Data.List (nub)
import qualified Data.Matrix as M
import Data.Maybe (fromMaybe)

type Plot = M.Matrix Char

data Direction = N | S | E | W
  deriving (Eq, Show, Ord)

type Coordinate = (Int, Int)

data Guard = Guard
  { direction :: Direction,
    pos :: Coordinate,
    history :: [(Coordinate, Direction)]
  }
  deriving (Show, Eq, Ord)

isLoop :: Guard -> Bool
isLoop guard = (pos guard, direction guard) `elem` history guard

move :: Guard -> Guard
move guard = Guard d pos' history'
  where
    d = direction guard
    (y, x) = pos guard
    history' = ((y, x), d) : history guard
    pos' = case d of
      N -> (y - 1, x)
      E -> (y, x + 1)
      S -> (y + 1, x)
      W -> (y, x - 1)

turn :: Guard -> Guard
turn guard = guard {direction = direction'}
  where
    direction' = case direction guard of
      E -> S
      S -> W
      W -> N
      N -> E

step :: Plot -> Guard -> Maybe Guard
step plot guard = case current of
  Just '.' -> Just guard'
  Just '#' -> Just $ turn guard
  Just _ -> error "no-op"
  Nothing -> Nothing
  where
    guard' = move guard
    current = let (y, x) = pos guard' in M.safeGet y x plot

sim :: Plot -> Guard -> Guard
sim plot guard =
  fromMaybe (error "no-guard") $
    until
      (maybe True (maybe True isLoop . step plot))
      (\g -> g >>= step plot)
      (Just guard)

hasLoop :: Plot -> Guard -> Bool
hasLoop plot guard = maybe False isLoop (step plot guard')
  where
    guard' = sim plot guard

parse :: String -> (Plot, Guard)
parse text = (mtx', guard)
  where
    mtx = M.fromLists $ lines text
    g = find (\v -> snd v == '^') (M.toList $ M.mapPos (,) mtx)
    coordinate = fst $ fromMaybe (error "no-op") g
    guard = Guard N coordinate []
    mtx' = M.setElem '.' coordinate mtx

first :: String -> String
first input = show $ length coordinates
  where
    (plot, guard) = parse input
    guard' = sim plot guard
    coordinates = nub $ pos guard' : (fst <$> history guard')

second :: String -> String
second input = show $ length $ filter (`hasLoop` guard) plots
  where
    (plot, guard) = parse input
    guard' = sim plot guard
    coordinates = nub $ pos guard' : filter (/= pos guard) (fst <$> history guard')
    plots = (\c -> M.setElem '#' c plot) <$> coordinates