module Day14 (first, second) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Function (on)
import Data.List (group, groupBy, partition, sort, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Matrix as MX
import Debug.Trace (trace)

type Coordinate = (Int, Int)

type Bound = (Int, Int)

type Guard = (Coordinate, Coordinate)

move :: Bound -> Guard -> Guard
move (xlim, ylim) ((x, y), (dx, dy)) = (((x + dx) `mod` xlim, (y + dy) `mod` ylim), (dx, dy))

sim :: Bound -> [Guard] -> [Guard]
sim bound guards = move bound <$> guards

parse :: String -> [Guard]
parse text = readGuard <$> guards
  where
    guards = lines text
    readGuard line = ((read x, read y), (read dx, read dy))
      where
        [pos, vel] = words line
        [x, y] = splitOn "," (drop 2 pos)
        [dx, dy] = splitOn "," (drop 2 vel)

splitX :: Bound -> [Guard] -> [[Guard]]
splitX (xlim, _) guards = [left, filter (\((x, _), _) -> x > bound) right]
  where
    bound = xlim `div` 2
    (left, right) = partition (\((x, _), _) -> x < bound) guards

splitY :: Bound -> [Guard] -> [[Guard]]
splitY (_, ylim) guards = [left, filter (\((_, y), _) -> y > bound) right]
  where
    bound = ylim `div` 2
    (left, right) = partition (\((_, y), _) -> y < bound) guards

first :: String -> String
first input = show $ product $ concatMap (fmap length . splitY bound) (splitX bound guards')
  where
    guards = parse input
    bound = (101, 103)
    guards' = iterate (sim bound) guards !! 100

fromCoordinates :: Bound -> [Coordinate] -> MX.Matrix Char
fromCoordinates (xlim, ylim) = foldl (\mtx (x, y) -> MX.setElem '#' (succ y, succ x) mtx) init
  where
    init = '.' <$ MX.zero ylim xlim

hasSeq :: [Int] -> Bool
hasSeq row = any (\g -> length g >= 30) $ group $ zipWith (-) (tail $ sort row) (init $ sort row)

second :: String -> String
second input = show $ trace image idx
  where
    guards = parse input
    bound = (101, 103)
    image = unlines . MX.toLists $ fromCoordinates bound (fst <$> guards')
    (idx, guards') =
      until
        (\(_, g) -> any (\row -> hasSeq $ fst <$> row) $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) $ fst <$> g)
        (bimap succ (sim bound))
        (0, guards)