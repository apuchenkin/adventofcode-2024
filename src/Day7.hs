module Day7 (first, second) where

import Data.List.Split (splitOn)

parse :: String -> [(Int, [Int])]
parse text = parseLine <$> lines text
  where
    parseLine line = (read left, read <$> splitOn " " right)
      where
        [left, right] = splitOn ": " line

isValid :: (Int, [Int]) -> Bool
isValid (_, []) = error "no-op"
isValid (result, x : xs) = result `elem` matches
  where
    matches = foldl (\acc v -> [(+) v, (*) v] <*> acc) [x] xs

isValid2 :: (Int, [Int]) -> Bool
isValid2 (_, []) = error "no-op"
isValid2 (result, x : xs) = result `elem` matches
  where
    matches = foldl (\acc v -> [(+) v, (*) v, \x -> read $ show x ++ show v] <*> acc) [x] xs

first :: String -> String
first input = show $ sum $ fst <$> filter isValid (parse input)

second :: String -> String
second input = show $ sum $ fst <$> filter isValid2 (parse input)