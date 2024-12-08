module Day8 (first, second) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Matrix as MX
import Data.Maybe (isJust)

parse :: String -> MX.Matrix Char
parse text = MX.fromLists $ lines text

antinodes :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodes (x, y) (x', y') = [left, right]
  where
    left = (x - (x' - x), y - (y' - y))
    right = (x' + (x' - x), y' + (y' - y))

complements :: [(Int, Int)] -> [(Int, Int)]
complements xs = coordinates
  where
    pairs = (,) <$> xs <*> xs
    coordinates = concatMap (filter (`notElem` xs) . uncurry antinodes) pairs

antinodes2 :: MX.Matrix a -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodes2 mtx (x, y) (x', y')
  | x' == x && y' == y = [(x, y), (x', y')]
  | otherwise = takeWhile (inside mtx) lefts ++ takeWhile (inside mtx) rights
  where
    xs = (*) (x' - x) <$> [1 ..]
    ys = (*) (y' - y) <$> [1 ..]
    lefts = bimap (x -) (y -) <$> zip xs ys
    rights = bimap (x' +) (y' +) <$> zip xs ys

complements2 :: MX.Matrix a -> [(Int, Int)] -> [(Int, Int)]
complements2 mtx xs = nub $ filter (inside mtx) coordinates
  where
    pairs = (,) <$> xs <*> xs
    coordinates = concatMap (uncurry (antinodes2 mtx)) pairs

inside :: MX.Matrix a -> (Int, Int) -> Bool
inside mtx (x, y) = isJust (MX.safeGet x y mtx)

first :: String -> String
first input = show $ length $ filter (inside mtx) antinodes
  where
    mtx = parse input
    coordinates = filter ((/= '.') . snd) $ MX.toList $ MX.mapPos (,) mtx
    cmap = M.fromListWith (++) $ (\(a, b) -> (b, [a])) <$> coordinates
    antinodes = nub $ concatMap snd $ M.toList $ complements <$> cmap

second :: String -> String
second input = show $ length antinodes
  where
    mtx = parse input
    coordinates = filter ((/= '.') . snd) $ MX.toList $ MX.mapPos (,) mtx
    cmap = M.fromListWith (++) $ (\(a, b) -> (b, [a])) <$> coordinates
    antinodes = nub $ concatMap snd $ M.toList $ complements2 mtx <$> cmap