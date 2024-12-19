module Day19 (first, second) where

import Algorithm.Search (bfs, pruning)
import Control.Monad (foldM)
import qualified Control.Monad.State as S
import Data.List (isPrefixOf, partition)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

type Cache = M.Map String Int

parse :: String -> ([String], [String])
parse text = (splitOn ", " left, lines right)
  where
    [left, right] = splitOn "\n\n" text

isValid :: String -> String -> Bool
isValid target option = length option <= length target && isPrefixOf option target

compose :: [String] -> String -> Maybe [String]
compose options target =
  bfs
    (moves options `pruning` (not . isValid target))
    (== target)
    ""

moves :: (Functor f) => f [a] -> [a] -> f [a]
moves options start = (start ++) <$> options

findPaths :: [String] -> String -> String -> S.State Cache Int
findPaths options start target
  | target == start = return 1
  | otherwise = do
      cache <- S.gets $ M.lookup start
      case cache of
        Just r -> return r
        Nothing -> do
          let (left, right) = partition (== target) (moves options start)
          let options' = filter (isValid target) right
          rest <- foldM (\acc p -> (+ acc) <$> findPaths options p target) 0 options'
          let result = length left + rest
          _ <- S.modify $ M.insert start result
          return result

first :: String -> String
first input = show $ length $ mapMaybe (compose options) candidates
  where
    (options, candidates) = parse input

second :: String -> String
second input = show $ sum $ (\s -> S.evalState (findPaths options "" s) M.empty) <$> candidates
  where
    (options, candidates) = parse input