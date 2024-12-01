module Day1 (first, second) where

import Data.List (sort)

parse :: String -> ([Int], [Int])
parse text = (fst <$> pairs, snd <$> pairs)
  where
    res = lines text
    pairs = parseLine <$> res
    parseLine line = (read left, read right)
      where
        [left, right] = words line

first :: String -> String
first input = show $ sum distances
  where
    (left, right) = parse input
    distances = abs <$> zipWith (-) (sort right) (sort left)

second :: String -> String
second input = show $ sum $ process <$> left
  where
    (left, right) = parse input
    process x = x * length (filter (== x) right)