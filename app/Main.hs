module Main where

import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import System.Environment (getArgs)

type Solution a = String -> a

data Day a = Day
  { first :: Solution a,
    second :: Solution a
  }

getDay :: String -> Day (IO String)
getDay idx = case idx of
  "1" -> Day (return . Day1.first) (return . Day1.second)
  "2" -> Day (return . Day2.first) (return . Day2.second)
  "3" -> Day (return . Day3.first) (return . Day3.second)
  "4" -> Day (return . Day4.first) (return . Day4.second)
  "5" -> Day (return . Day5.first) (return . Day5.second)
  "6" -> Day (return . Day6.first) (return . Day6.second)
  "7" -> Day (return . Day7.first) (return . Day7.second)
  "8" -> Day (return . Day8.first) (return . Day8.second)
  "9" -> Day (return . Day9.first) (return . Day9.second)
  "10" -> Day (return . Day10.first) (return . Day10.second)
  "11" -> Day (return . Day11.first) (return . Day11.second)
  "12" -> Day (return . Day12.first) (return . Day12.second)
  "13" -> Day (return . Day13.first) (return . Day13.second)
  "14" -> Day (return . Day14.first) (return . Day14.second)
  "15" -> Day (return . Day15.first) (return . Day15.second)
  "16" -> Day (return . Day16.first) (return . Day16.second)
  "17" -> Day (return . Day17.first) (return . Day17.second)
  "18" -> Day (return . Day18.first) (return . Day18.second)
  "19" -> Day (return . Day19.first) (return . Day19.second)
  "20" -> Day (return . Day20.first) (return . Day20.second)
  "21" -> Day (return . Day21.first) (return . Day21.second)
  "22" -> Day (return . Day22.first) (return . Day22.second)
  "23" -> Day (return . Day23.first) (return . Day23.second)
  "24" -> Day (return . Day24.first) (return . Day24.second)
  "25" -> Day (return . Day25.first) (return . Day25.second)
  _ -> error "day unsupported"

main :: IO ()
main = do
  args <- getArgs
  let idx = head args
  let day = getDay idx
  file <- readFile $ "input/day" ++ idx ++ ".txt"
  first' <- first day file
  putStrLn first'
  second' <- second day file
  putStrLn second'