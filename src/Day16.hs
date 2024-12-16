module Day16 (first, second) where

import Algorithm.Search (dijkstraAssoc)
import Data.Foldable (find)
import Data.List (nub)
import qualified Data.Matrix as MX
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)

type Coordinate = (Int, Int)

data Direction = N | S | E | W
  deriving (Eq, Show, Ord)

type Plot = MX.Matrix Char

data Move = FW | L | R
  deriving (Eq, Show, Ord)

data Reindeer = Reindeer
  { pos :: Coordinate,
    d :: Direction,
    history :: [Move],
    coordinates :: [(Coordinate, Direction)]
  }
  deriving (Show)

instance Eq Reindeer where
  l == r
    | pos l /= pos r = False
    | d l /= d r = False
    | cost (history l) == cost (history r) && history l /= history r = False
    | otherwise = True

instance Ord Reindeer where
  l <= r = (pos l, d l, history l) <= (pos r, d r, history r)

cost :: [Move] -> Int
cost moves = sum $ cost' <$> moves
  where
    cost' FW = 1
    cost' _ = 1000

turnL :: Reindeer -> Reindeer
turnL deer = deer {d = d', history = L : history deer}
  where
    d' = case d deer of
      N -> W
      W -> S
      S -> E
      E -> N

turnR :: Reindeer -> Reindeer
turnR deer = deer {d = d', history = L : history deer}
  where
    d' = case d deer of
      N -> E
      E -> S
      S -> W
      W -> N

mv :: Direction -> Coordinate -> Coordinate
mv d (y, x) = case d of
  N -> (y - 1, x)
  E -> (y, x + 1)
  S -> (y + 1, x)
  W -> (y, x - 1)

canMove :: Plot -> Reindeer -> Bool
canMove mtx deer = case mtx MX.! pos deer' of
  '.' -> True
  _ -> False
  where
    deer' = move deer

move :: Reindeer -> Reindeer
move deer = deer {pos = mv (d deer) (pos deer), history = FW : history deer}

act :: Reindeer -> Move -> Reindeer
act deer action = deer' {coordinates = (pos deer, d deer) : coordinates deer}
  where
    deer' = case action of
      FW -> move deer
      L -> turnL deer
      R -> turnR deer

actions :: Plot -> Reindeer -> [Move]
actions mtx deer = fst <$> filter snd [fw, l, r]
  where
    last = if null (history deer) then Nothing else Just $ head (history deer)
    fw = (FW, canMove mtx deer)
    l = (L, last /= Just R)
    r = (R, last /= Just L)

parse :: String -> (Coordinate, Reindeer, Plot)
parse text = (end, deer, mtx')
  where
    mtx = MX.fromLists $ lines text
    end = maybe (0, 0) fst $ find (\(_, c) -> c == 'E') $ MX.mapPos (,) mtx
    start = maybe (0, 0) fst $ find (\(_, c) -> c == 'S') $ MX.mapPos (,) mtx
    mtx' = MX.setElem '.' start $ MX.setElem '.' end mtx
    deer = Reindeer start E [] []

first :: String -> String
first input = show val
  where
    (end, deer, mtx) = parse input
    (val, _) = findPath mtx (\d -> pos d == end) deer

findPath :: Plot -> (Reindeer -> Bool) -> Reindeer -> (Int, Reindeer)
findPath mtx isTarget deer = (value, last path)
  where
    (value, path) =
      fromMaybe (0, []) $
        dijkstraAssoc
          (\deer -> (\a -> (act deer a, cost [a])) <$> actions mtx deer)
          isTarget
          deer

second :: String -> String
second input = show $ length $ nub $ concatMap ((\p -> pos p : (fst <$> coordinates p)) . snd) (filter ((== value) . fst) result)
  where
    (end, deer, mtx) = parse input
    (value, path) = findPath mtx (\d -> pos d == end) deer
    result =
      until
        (\paths -> traceShow (length paths, length $ nub $ concatMap (fmap fst . coordinates . snd) paths) $ any ((/= value) . fst) paths)
        (\paths -> findPath mtx (\d -> (pos d == end) && history d `notElem` (history . snd <$> paths)) deer : paths)
        [(value, path)]