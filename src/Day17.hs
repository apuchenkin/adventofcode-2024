module Day17 (first, second) where

import Control.Monad (unless, when)
import qualified Control.Monad.State as ST
import Data.Bits (Bits (xor))
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as M
import Debug.Trace (traceShowId)

data Dest = Val Int | Reg Register
  deriving (Show, Eq, Ord)

data Register = A | B | C
  deriving (Show, Eq, Ord)

data Instruction = ADV Dest | BXL Int | BST Dest | JNZ Int | BXC | OUT Dest | BDV Dest | CDV Dest
  deriving (Show, Eq, Ord)

type Registers = M.Map Register Int

data Mem = Mem
  { ins :: [Instruction],
    cursor :: Int,
    reg :: Registers,
    output :: [Int]
  }
  deriving (Show)

readOperand :: Int -> Dest
readOperand val = case val of
  0 -> Val 0
  1 -> Val 1
  2 -> Val 2
  3 -> Val 3
  4 -> Reg A
  5 -> Reg B
  6 -> Reg C
  _ -> error "readOperand"

readInstruction :: Int -> Int -> Instruction
readInstruction byte val = case byte of
  0 -> ADV dest
  1 -> BXL val
  2 -> BST dest
  3 -> JNZ val
  4 -> BXC
  5 -> OUT dest
  6 -> BDV dest
  7 -> CDV dest
  _ -> error "readInstruction"
  where
    dest = readOperand val

val :: Dest -> ST.State Mem Int
val (Val v) = return v
val (Reg r) = ST.gets $ (M.! r) . reg

adv :: Dest -> ST.State Mem ()
adv dest = do
  num <- ST.gets $ (M.! A) . reg
  denum <- val dest
  let result = num `div` (2 ^ denum)
  _ <- ST.modify (\s -> s {reg = M.insert A result (reg s)})
  return ()

jnz :: Int -> ST.State Mem ()
jnz v = do
  num <- ST.gets $ (M.! A) . reg
  unless (num == 0) $ ST.modify (\s -> s {cursor = v})
  when (num == 0) $ ST.modify (\s -> s {cursor = succ (cursor s)})

out :: Dest -> ST.State Mem ()
out dest = do
  v <- val dest
  _ <- ST.modify (\s -> s {output = v `mod` 8 : output s})
  return ()

bst :: Dest -> ST.State Mem ()
bst dest = do
  v <- val dest
  let result = v `mod` 8
  _ <- ST.modify (\s -> s {reg = M.insert B result (reg s)})
  return ()

bxl :: Int -> ST.State Mem ()
bxl v = do
  num <- ST.gets $ (M.! B) . reg
  let result = v `xor` num
  _ <- ST.modify (\s -> s {reg = M.insert B result (reg s)})
  return ()

bxc :: ST.State Mem ()
bxc = do
  vb <- ST.gets $ (M.! B) . reg
  vc <- ST.gets $ (M.! C) . reg
  let result = vb `xor` vc
  _ <- ST.modify (\s -> s {reg = M.insert B result (reg s)})
  return ()

cdv :: Dest -> ST.State Mem ()
cdv dest = do
  num <- ST.gets $ (M.! A) . reg
  denum <- val dest
  let result = num `div` (2 ^ denum)
  _ <- ST.modify (\s -> s {reg = M.insert C result (reg s)})
  return ()

eval :: Instruction -> ST.State Mem ()
eval instruction = case instruction of
  ADV dest -> adv dest
  OUT dest -> out dest
  JNZ v -> jnz v
  BST dest -> bst dest
  BXL v -> bxl v
  CDV dest -> cdv dest
  BXC -> bxc
  ins -> error $ show ins

isJNZ :: Instruction -> Bool
isJNZ (JNZ _) = True
isJNZ _ = False

step :: ST.State Mem ()
step = do
  instruction <- ST.gets (\s -> ins s !! cursor s)
  eval instruction
  unless (isJNZ instruction) $ ST.modify (\s -> s {cursor = succ (cursor s)})

sim :: ST.State Mem ()
sim = do
  c <- ST.gets cursor
  len <- ST.gets (length . ins)
  unless (c < 0 || c >= len) (step >> sim)

parse :: String -> (Registers, [Instruction])
parse text = (registers, instructions)
  where
    [left, right] = splitOn "\n\n" text
    [a, b, c] = read . drop 12 <$> lines left
    registers = M.fromList [(A, a), (B, b), (C, c)]
    instructions = (\[a, b] -> readInstruction (read a) (read b)) <$> chunksOf 2 (splitOn "," (drop 9 right))

first :: String -> String
first input = show $ reverse $ output result
  where
    (reg, instructions) = parse input
    init = Mem instructions 0 reg []
    result = ST.execState sim init

parseProgram :: String -> [Int]
parseProgram text = read <$> splitOn "," (drop 9 right)
  where
    [_, right] = splitOn "\n\n" text

simProgram :: [Int] -> ST.State Mem Bool
simProgram program = do
  c <- ST.gets cursor
  len <- ST.gets (length . ins)
  result <- ST.gets (reverse . output)
  let isProgram = and $ zipWith (==) result program
  if not (null result) && not isProgram
    then return False
    else do
      if c < 0 || c >= len
        then return $ traceShowId result == program
        else do
          _ <- step
          simProgram program

rng :: (Int, Int) -> [Int]
rng (a, b) = [a, a + ((b - a) `div` 100000) .. b]

solver :: [Int] -> Mem -> (Int, Int) -> Int -> Int
solver program init (min, max) i = if i >= 3 then minR else solver program init (minR, maxR) (i + 1)
  where
    range =
      (\r -> let st = init {reg = M.insert A r (reg init)} in (r, reverse . output $ ST.execState sim st))
        <$> rng (min, max)
    idx = filter (\((idx, out), _) -> take (6 + i) (reverse out) == take (6 + i) (reverse program)) $ zip range [0 ..]
    (minR, maxR) = traceShowId $ (fst $ range !! pred (snd (head idx)), fst $ range !! succ (snd (last idx)))

second :: String -> String
second input = show result
  where
    program = parseProgram input
    (registers, instructions) = parse input
    init = Mem instructions 0 (M.insert A (8 ^ 15) registers) []
    start = solver program init (8 ^ 15, 8 ^ 16) 0
    result =
      until
        (\r -> let st = init {reg = M.insert A r (reg init)} in ST.evalState (simProgram program) st)
        succ
        (traceShowId start)