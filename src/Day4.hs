module Day4 (first, second) where

import qualified Data.Matrix as M
import Data.Maybe (catMaybes)

parse :: String -> M.Matrix Char
parse text = M.fromLists res
  where
    res = lines text

rotate45 :: M.Matrix Char -> M.Matrix (Maybe Char)
rotate45 mtx = foldl (\m (c, v) -> M.setElem (Just v) c m) initial coordinates
  where
    rotate (y, x) = (M.nrows mtx + y - x, y + x)
    coordinates = M.mapPos (\c v -> (rotate c, v)) mtx
    initial = M.matrix (2 * M.nrows mtx) (2 * M.ncols mtx) (const Nothing)

substrings :: String -> String -> Int
substrings _ [] = 0
substrings string (x : xs) = if isEqual then 1 + substrings string xs else substrings string xs
  where
    isGreater = length (x : xs) >= length string
    isEqual = isGreater && and (zipWith (==) string (x : xs))

variations :: M.Matrix Char -> [M.Matrix (Maybe Char)]
variations mtx =
  [ Just <$> mtx,
    Just <$> flipM mtx,
    Just <$> M.transpose mtx,
    Just <$> flipM (M.transpose mtx),
    rotated,
    flipM rotated,
    M.transpose rotated,
    flipM $ M.transpose rotated
  ]
  where
    rotated = rotate45 mtx

first :: String -> String
first input = show $ sum $ sum . fmap (substrings "XMAS") <$> vars
  where
    plot = parse input
    vars = fmap catMaybes . M.toLists <$> variations plot

xpattern :: M.Matrix Char
xpattern = M.fromLists ["M.S", ".A.", "M.S"]

eqPattern :: M.Matrix Char -> Bool
eqPattern m = and $ M.toList meqs
  where
    meqs = M.elementwise (\x y -> (x == '.') || x == y) xpattern m

flipM :: M.Matrix a -> M.Matrix a
flipM mtx = M.fromLists $ reverse <$> M.toLists mtx

vars2 :: M.Matrix a -> [M.Matrix a]
vars2 mtx = [mtx, flipM mtx, mtx', flipM mtx']
  where
    mtx' = M.transpose mtx

second :: String -> String
second input = show $ length $ filter (any eqPattern . vars2) mtx'
  where
    mtx = parse input
    mtx' = concatMap (\y -> (\x -> M.submatrix x (x + 2) y (y + 2) mtx) <$> [1 .. M.ncols mtx - 2]) [1 .. M.nrows mtx - 2]