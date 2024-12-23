{-# LANGUAGE LambdaCase #-}

module Day22 (first, second) where

import qualified Control.Monad.State as S
import Data.Bits (Bits (xor))
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Debug.Trace (traceShow)

type Pattern = [Int]

mix :: Int -> Int -> Int
mix = xor

prune :: (Integral a) => a -> a
prune a = a `mod` 16777216

evolve :: Int -> Int
evolve n = n'''
  where
    n' = prune $ (n * 64) `mix` n
    n'' = prune $ (n' `div` 32) `mix` n'
    n''' = prune $ (n'' * 2048) `mix` n''

parse :: String -> [Int]
parse text = read <$> lines text

first :: String -> String
first input = show $ sum $ (\i -> iterate evolve i !! 2000) <$> n
  where
    n = parse input

lastDigit :: Int -> Int
lastDigit n = n `mod` 10

sequences :: [(Int, Int)] -> S.Set Pattern
sequences byer = S.map (fmap snd) seqs
  where
    maxPrice = maximum $ fst <$> byer
    (_, seqs) =
      until
        (\((_, b), _) -> null b)
        (\((a, b), z) -> ((tail a ++ [head b], tail b), if fst (last a) == maxPrice then S.insert a z else z))
        (splitAt 4 byer, S.empty)

type Cache = M.Map (Int, Pattern) (Maybe Int)

findPattern :: (a -> b -> Bool) -> [a] -> [b] -> Maybe a
findPattern eqs sequence pattern = last . fst <$> result
  where
    result =
      until
        ( \case
            Nothing -> True
            Just (a, _) -> and $ zipWith eqs a pattern
        )
        ( \case
            Nothing -> Nothing
            Just (_, []) -> Nothing
            Just (left, x : xs) -> Just (tail left ++ [x], xs)
        )
        (Just $ splitAt 4 sequence)

eval' :: Int -> Pattern -> S.State Cache (Maybe Int)
eval' byer pattern = do
  let key = (byer, pattern)
  cache <- S.gets $ M.lookup key
  case cache of
    Just result -> return result
    Nothing -> do
      let changes = computeChanges byer
      let result = fst <$> findPattern (\a b -> snd a == b) changes pattern
      _ <- S.modify $ M.insert key result
      return $ result

computeChanges :: Int -> [(Int, Int)]
computeChanges byer = changes
  where
    prices = lastDigit <$> take 2000 (iterate evolve byer)
    changes = zip prices (0 : zipWith (-) (tail prices) (init prices))

eval :: [Int] -> Pattern -> S.State Cache Int
eval byers pattern = sum . catMaybes <$> res
  where
    res = mapM (`eval'` pattern) byers

second :: String -> String
second input = show $ maximum $ S.evalState vals M.empty
  where
    byers = parse input
    seqs = S.toList $ foldl (\a b -> a `S.union` sequences b) S.empty (computeChanges <$> byers)
    vals = mapM (\(pattern, idx) -> traceShow idx $ eval byers pattern) (zip seqs [0 ..])