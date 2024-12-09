module Day9 (first, second) where

import qualified Data.Bifunctor as BF (second)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)

parse :: String -> [(Int, Maybe Int)]
parse text = (\(size, index) -> if odd index then (size, Nothing) else (size, Just $ index `div` 2)) <$> zip digits indexes
  where
    digits = read . (: []) <$> text :: [Int]
    indexes = [0 ..] :: [Int]

fillSpace :: Int -> [(Int, Maybe Int)] -> ([(Int, Maybe Int)], [(Int, Maybe Int)])
fillSpace 0 xs = ([], xs)
fillSpace size [] = ([(size, Nothing)], [])
fillSpace size [(blockSize, blockId)]
  | size >= blockSize = ([(blockSize, blockId), (size - blockSize, Nothing)], [])
  | otherwise = ([(size, blockId)], [(blockSize - size, blockId)])
fillSpace size (block : xs) = (resolved, remaining ++ xs)
  where
    (resolved, remaining) = fillSpace size [block]

process :: [(Int, Maybe Int)] -> ([(Int, Int)], [(Int, Maybe Int)])
process [] = ([], [])
process [(_, Nothing)] = ([], [])
process [(size, Just block)] = ([(size, block)], [])
process ((size, Just block) : xs) = ((size, block) : left, right)
  where
    (left, right) = process xs
process ((size, Nothing) : xs) = process $ left ++ reverse right
  where
    (left, right) = fillSpace size (reverse xs)

decompress :: [(Int, Int)] -> [Int]
decompress = concatMap (uncurry replicate)

checksum :: [Int] -> Int
checksum disk = sum $ zipWith (*) disk [0 ..]

first :: String -> String
first input = show $ checksum $ decompress result
  where
    disk = parse input
    (result, _) =
      until
        (\(_, xs) -> null xs)
        (\(acc, xs) -> let (acc', xs') = process xs in (acc ++ acc', xs'))
        ([], disk)

cutSpace :: (Int, Maybe Int) -> (Int, Maybe Int) -> (Int, Maybe Int)
cutSpace (fn, _) (space, _) = (space - fn, Nothing)

moveFile :: Int -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
moveFile fdx disk = disk'
  where
    (left, file : right) = break (\(_, block) -> block == Just fdx) disk
    (left', right') = break (\b -> isNothing (snd b) && fst b >= fst file) left
    disk' = case right' of
      [] -> left' ++ file : right
      [space] -> left' ++ file : space : right
      (space : xs) -> left' ++ file : cutSpace file space : xs ++ (fst file, Nothing) : right

second :: String -> String
second input = show $ checksum $ decompress $ BF.second (fromMaybe 0) <$> result
  where
    disk = parse input
    idMax = maximum $ mapMaybe snd disk
    result = foldl (flip moveFile) disk (reverse [0 .. idMax])