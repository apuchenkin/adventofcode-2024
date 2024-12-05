module Day5 (first, second) where

import Data.List (elemIndex, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, isNothing)

type Rule = (Int, Int)

type Page = [Int]

type Input = ([Rule], [Page])

parse :: String -> Input
parse text = (parseRule <$> lines rules, fmap read . splitOn "," <$> lines pages)
  where
    [rules, pages] = splitOn "\n\n" text
    parseRule line = (read left, read right)
      where
        [left, right] = splitOn "|" line

matchRule :: Page -> Rule -> Bool
matchRule page (left, right)
  | isNothing leftIdx = True
  | isNothing rightIdx = True
  | otherwise = leftIdx <= rightIdx
  where
    leftIdx = elemIndex left page
    rightIdx = elemIndex right page

middle :: [a] -> a
middle list = list !! (length list `div` 2)

first :: String -> String
first input = show $ sum $ middle <$> matches
  where
    (rules, pages) = parse input
    matches = filter (\p -> all (matchRule p) rules) pages

order :: [Rule] -> Page -> Page
order rules = sortBy predicate
  where
    predicate left right
      | isJust $ elemIndex (left, right) rules = LT
      | isJust $ elemIndex (right, left) rules = GT
      | otherwise = EQ

second :: String -> String
second input = show $ sum $ middle . order rules <$> matches
  where
    (rules, pages) = parse input
    matches = filter (\page -> not $ all (matchRule page) rules) pages