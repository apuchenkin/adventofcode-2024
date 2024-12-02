module Day2 (first, second) where

import Control.Monad (liftM2)

parse :: String -> [[Int]]
parse text = fmap read . words <$> res
  where
    res = lines text

isSafe :: [Int] -> Bool
isSafe line = (isPositive diffs || isNegative diffs) && inRange diffs
  where
    diffs = zipWith (-) (init line) (tail line)
    isPositive = all (> 0)
    isNegative = all (< 0)
    inRange = all (liftM2 (&&) (>= 1) (<= 3) . abs)

first :: String -> String
first input = show $ length $ filter isSafe ranges
  where
    ranges = parse input

second :: String -> String
second input = show $ length $ filter (\l -> isSafe l || any isSafe (sublists l)) ranges
  where
    ranges = parse input
    dropAt k l = take k l ++ drop (k + 1) l
    sublists list = flip dropAt list <$> [0 .. length list]