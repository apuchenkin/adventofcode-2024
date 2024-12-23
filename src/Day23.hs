module Day23 (first, second) where

import Data.List (group, isPrefixOf, nub, sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Tuple (swap)
import Debug.Trace (traceShowId)

parse :: String -> M.Map String [String]
parse text = foldl (\mp p -> insert (insert mp p) (swap p)) M.empty pairs
  where
    pairs = readLine <$> lines text
    insert mp (a, b) = M.alter (\k -> if isNothing k then Just [b] else (b :) <$> k) a mp
    readLine line = (left, right)
      where
        [left, right] = splitOn "-" line

get3Conn :: M.Map String [String] -> String -> [[String]]
get3Conn mtx start = connections
  where
    mid = fromMaybe [] (mtx M.!? start)
    connections = nub $ concatMap (\m -> (\e -> sort [start, m, e]) <$> filter (\e -> maybe False (start `elem`) (mtx M.!? e)) (fromMaybe [] (mtx M.!? m))) mid

first :: String -> String
first input = show $ length $ filter (any ("t" `isPrefixOf`)) connections
  where
    mtx = parse input
    connections = nub $ concatMap (get3Conn mtx) (M.keys mtx)

getInnConn :: M.Map String [String] -> String -> [String]
getInnConn mtx start = if null mids' then [] else head mids'
  where
    nodes = fromMaybe [] (mtx M.!? start)
    getConnected v = sort $ v : filter (\m -> maybe False (m `elem`) (mtx M.!? v)) (start : filter (/= v) nodes)
    groups = (\g -> (head g, length g)) <$> group (getConnected <$> nodes)
    mids' = fst <$> filter (\(g, count) -> count >= 12) groups

second :: String -> String
second input = show $ length $ traceShowId connections
  where
    mtx = parse input
    connections = filter (not . null) $ getInnConn mtx <$> M.keys mtx