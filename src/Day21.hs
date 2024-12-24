module Day21 (first, second) where

import Algorithm.Search (pruning)
import Control.Monad (zipWithM)
import qualified Control.Monad.State as S
import Data.Foldable (find)
import Data.List (group, sort)
import qualified Data.Map as M
import qualified Data.Matrix as MX

type Coordinate = (Int, Int)

type Plot = MX.Matrix Char

type Path = [Direction]

type Cache = M.Map (Coordinate, [(Coordinate, Direction)]) [Path]

data Direction = S | E | W | N
  deriving (Eq, Show, Ord)

type Memo = M.Map (String, Int) Int

mv :: Direction -> Coordinate -> Coordinate
mv d (y, x) = case d of
  N -> (y - 1, x)
  E -> (y, x + 1)
  S -> (y + 1, x)
  W -> (y, x - 1)

keypad :: MX.Matrix Char
keypad =
  MX.fromLists
    [ "789",
      "456",
      "123",
      "#0A"
    ]

directionalKeypad :: MX.Matrix Char
directionalKeypad =
  MX.fromLists
    [ "#^A",
      "<v>"
    ]

parse :: String -> [String]
parse text = res
  where
    res = lines text

getCoordinate :: (Eq a) => MX.Matrix a -> a -> (Int, Int)
getCoordinate mtx target = maybe (0, 0) fst $ find (\(_, c) -> c == target) $ MX.mapPos (,) mtx

isValid :: Plot -> Coordinate -> Bool
isValid mtx (y, x) = case MX.safeGet y x mtx of
  Just '#' -> False
  Just _ -> True
  _ -> False

showPath :: [Direction] -> String
showPath dir = (showD <$> dir) ++ "A"
  where
    showD d = case d of
      N -> '^'
      S -> 'v'
      W -> '<'
      E -> '>'

moves :: Plot -> (Coordinate, [(Coordinate, Direction)]) -> [(Coordinate, [(Coordinate, Direction)])]
moves mtx =
  (\(c, h) -> (\m -> (mv m c, (c, m) : h)) <$> [S, E, W, N])
    `pruning` (\(c, h) -> not (isValid mtx c) || c `elem` (fst <$> h) || length (group (sort (snd <$> h))) > 2)

findPaths :: Plot -> (Coordinate, [(Coordinate, Direction)]) -> Coordinate -> S.State Cache [Path]
findPaths mtx start target
  | target == fst start = return [reverse $ snd <$> snd start]
  | otherwise = do
      cache <- S.gets $ M.lookup start
      case cache of
        Just r -> return r
        Nothing -> do
          let candidates = moves mtx start
          result <- mapM (\start' -> findPaths mtx start' target) candidates
          _ <- S.modify $ M.insert start (concat result)
          return (concat result)

getMinPathLength :: Int -> Plot -> String -> S.State Memo Int
getMinPathLength n mtx path = case n of
  0 -> return $ length path
  _ -> do
    cache <- S.gets $ M.lookup (path, n)
    case cache of
      Just r -> return r
      Nothing -> do
        result <- minLength
        _ <- S.modify $ M.insert (path, n) result
        return result
  where
    nodes = getCoordinate mtx <$> 'A' : path
    paths start end = do
      let paths' = showPath <$> S.evalState (findPaths mtx (start, []) end) M.empty
      lengts <- mapM (getMinPathLength (pred n) directionalKeypad) paths'
      return $ minimum lengts
    minLength = sum <$> zipWithM paths (init nodes) (tail nodes)

first :: String -> String
first input = show $ sum $ zipWith (\code size -> read (take 3 code) * size) codes lengths
  where
    codes = parse input
    lengths = (\code -> S.evalState (getMinPathLength 3 keypad code) M.empty) <$> codes

second :: String -> String
second input = show $ sum $ zipWith (\code size -> read (take 3 code) * size) codes lengths
  where
    codes = parse input
    lengths = (\code -> S.evalState (getMinPathLength 26 keypad code) M.empty) <$> codes