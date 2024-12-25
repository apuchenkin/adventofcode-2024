module Day25 (first, second) where

import Data.List (group, partition)
import Data.List.Split (splitOn)
import qualified Data.Matrix as MX

parse :: String -> [MX.Matrix Char]
parse text = MX.fromLists . lines <$> splitOn "\n\n" text

convert :: MX.Matrix Char -> [Int]
convert mtx = head . fmap (pred . length) . group <$> cols
  where
    cols = MX.toLists $ MX.transpose mtx

isOverlap :: MX.Matrix Char -> MX.Matrix Char -> Bool
isOverlap left right = not . or $ MX.toList $ MX.elementwise (\a b -> a == '#' && b == '#') left right

first :: String -> String
first input = show $ length $ filter id $ isOverlap <$> locks <*> keys
  where
    grids = parse input
    (locks, keys) = partition (all (== '#') . MX.getRow 1) grids

second :: String -> String
second input = show $ convert <$> n
  where
    n = parse input