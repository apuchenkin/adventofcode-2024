module Day13 (first, second) where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

newtype Coordinate = Coordinate (Int, Int)
  deriving (Show, Eq)

instance Num Coordinate where
  Coordinate (x, y) + Coordinate (x', y') = Coordinate (x + x', y + y')
  Coordinate (x, y) * Coordinate (x', y') = Coordinate (x * x', y * y')
  abs (Coordinate (x, y)) = Coordinate (abs x, abs y)

parse :: String -> [(Coordinate, Coordinate, Coordinate)]
parse text = process <$> chunks
  where
    chunks = splitOn "\n\n" text
    process chunk =
      (Coordinate (xa, ya), Coordinate (xb, yb), Coordinate (xp, yp))
      where
        [a, b, prize] = lines chunk
        [xa, ya] = read . drop 2 <$> (splitOn ", " $ drop 10 a)
        [xb, yb] = read . drop 2 <$> (splitOn ", " $ drop 10 b)
        [xp, yp] = read . drop 2 <$> (splitOn ", " $ drop 7 prize)

(<**>) :: Int -> Coordinate -> Coordinate
(<**>) n c = c * Coordinate (n, n)

getPrice :: (Coordinate, Coordinate, Coordinate) -> Maybe Int
getPrice (a, b, prize) = if null valid then Nothing else Just $ (\(x, y) -> x * 3 + y) (head valid)
  where
    candidates = (,) <$> [0 .. 100] <*> [0 .. 100] :: [(Int, Int)]
    valid = sort $ filter (\(x, y) -> (x <**> a) + (y <**> b) == prize) candidates

getPrice' :: (Coordinate, Coordinate, Coordinate) -> Maybe Int
getPrice' (a, b, prize) = if null costs then Nothing else Just $ minimum costs
  where
    Coordinate (xa, ya) = a
    Coordinate (xb, yb) = b
    Coordinate (x, y) = prize
    a' = fromIntegral ya / fromIntegral xa :: Rational
    b' = fromIntegral yb / fromIntegral xb :: Rational
    dx = (fromIntegral y - (b' * fromIntegral x)) / (a' - b')
    stepsA = filter (>= 0) $ let s = dx / fromIntegral xa in [floor s .. ceiling s]
    coordinates = (<**> a) <$> stepsA
    stepsB = concatMap (\(Coordinate (x', _)) -> let s = ((x - x') `div` xb) in [s .. s + 1]) coordinates :: [Int]
    moves = filter (\(moveA, moveB) -> Coordinate (x, y) == moveA <**> a + moveB <**> b) ((,) <$> stepsA <*> stepsB)
    costs = (\(a, b) -> 3 * a + b) <$> moves

offset :: Int
offset = 10000000000000

first :: String -> String
first input = show $ sum $ mapMaybe getPrice chunks
  where
    chunks = parse input

second :: String -> String
second input = show $ sum $ mapMaybe getPrice' chunks
  where
    chunks = (\(a, b, p) -> (a, b, Coordinate (offset, offset) + p)) <$> parse input