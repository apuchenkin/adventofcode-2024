module Day11 (first, second) where

import qualified Control.Monad.State as S
import qualified Data.Map as M

type Mem = M.Map (Int, Int) Int

step :: Int -> [Int]
step stone
  | stone == 0 = [1]
  | even size = read <$> [take half (show stone), drop half (show stone)]
  | otherwise = [stone * 2024]
  where
    size = length (show stone)
    half = size `div` 2

blink :: [Int] -> [Int]
blink = concatMap step

parse :: String -> [Int]
parse text = read <$> words text

sim :: Int -> Int -> S.State Mem Int
sim _ 0 = return 1
sim stone n = do
  m <- S.gets $ M.lookup (stone, n)
  case m of
    Just v -> return v
    Nothing -> do
      stones' <- mapM (`sim` pred n) (step stone)
      let result = sum stones'
      _ <- S.modify $ M.insert (stone, n) result
      return result

first :: String -> String
first input = show $ length (iterate blink stones !! 25)
  where
    stones = parse input

second :: String -> String
second input = show $ sum $ S.evalState (mapM (`sim` 75) stones) M.empty
  where
    stones = parse input