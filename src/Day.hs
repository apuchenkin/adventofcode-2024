module Day (first, second) where

import Debug.Trace (traceShowId)

parse :: String -> [String]
parse text = res
  where
    res = lines text

first :: String -> String
first input = show $ length $ traceShowId n
  where
    n = parse input

second :: String -> String
second input = show $ length $ traceShowId n
  where
    n = parse input