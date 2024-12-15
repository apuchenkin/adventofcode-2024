module Day15 (first, second) where

import Control.Monad (when)
import qualified Control.Monad.State as S
import Data.Foldable (find)
import Data.List.Split (splitOn)
import qualified Data.Matrix as MX

type Coordinate = (Int, Int)

data Direction = N | S | E | W
  deriving (Eq, Show, Ord)

type Plot = MX.Matrix Char

type Box = (Coordinate, Coordinate)

mv :: Coordinate -> Direction -> Coordinate
mv (y, x) d = case d of
  N -> (y - 1, x)
  E -> (y, x + 1)
  S -> (y + 1, x)
  W -> (y, x - 1)

canMove :: Plot -> Coordinate -> Direction -> Bool
canMove mtx (y, x) d = case MX.safeGet y x mtx of
  Nothing -> False
  Just '#' -> False
  Just 'O' -> canMove mtx c d
  Just '@' -> canMove mtx c d
  Just '.' -> True
  where
    c = mv (y, x) d

canMoveBox :: Plot -> Box -> Direction -> Bool
canMoveBox mtx (l, r) d = case d of
  N ->
    let ln = mv l d; rn = mv r d
     in case [mtx MX.! ln, mtx MX.! rn] of
          ".." -> True
          "]." -> canMoveBox mtx (mv ln W, ln) d
          ".[" -> canMoveBox mtx (rn, mv rn E) d
          "][" -> canMoveBox mtx (mv ln W, ln) d && canMoveBox mtx (rn, mv rn E) d
          "[]" -> canMoveBox mtx (ln, rn) d
          _ -> False
  S ->
    let ls = mv l d; rs = mv r d
     in case [mtx MX.! ls, mtx MX.! rs] of
          ".." -> True
          "]." -> canMoveBox mtx (mv ls W, ls) d
          ".[" -> canMoveBox mtx (rs, mv rs E) d
          "][" -> canMoveBox mtx (mv ls W, ls) d && canMoveBox mtx (rs, mv rs E) d
          "[]" -> canMoveBox mtx (ls, rs) d
          _ -> False
  W ->
    let w = mv l W
     in case mtx MX.! w of
          '.' -> True
          ']' -> canMoveBox mtx (mv w W, w) d
          _ -> False
  E ->
    let e = mv r E
     in case mtx MX.! e of
          '.' -> True
          '[' -> canMoveBox mtx (e, mv e E) d
          _ -> False

move :: Plot -> Coordinate -> Direction -> Plot
move mtx c d = case mtx MX.! c of
  '.' -> mtx
  '#' -> mtx
  'O' -> if canMove mtx c d then mtx' else mtx
  where
    c' = mv c d
    next = mtx MX.! c'
    mtx' = case next of
      '.' -> MX.setElem '.' c $ MX.setElem 'O' c' mtx
      '#' -> mtx
      'O' -> if canMove mtx c' d then move (move mtx c' d) c d else mtx

moveBox :: Plot -> Box -> Direction -> Plot
moveBox mtx (l, r) d = case d of
  N ->
    let ln = mv l d; rn = mv r d
     in case [mtx MX.! ln, mtx MX.! rn] of
          ".." -> MX.setElem '[' ln $ MX.setElem ']' rn $ MX.setElem '.' r $ MX.setElem '.' l mtx
          "]." -> moveBox (moveBox mtx (mv ln W, ln) d) (l, r) d
          ".[" -> moveBox (moveBox mtx (rn, mv rn E) d) (l, r) d
          "][" -> moveBox (moveBox (moveBox mtx (rn, mv rn E) d) (mv ln W, ln) d) (l, r) d
          "[]" -> moveBox (moveBox mtx (ln, rn) d) (l, r) d
          _ -> error $ "moveBox:" ++ show d
  S ->
    let ls = mv l d; rs = mv r d
     in case [mtx MX.! ls, mtx MX.! rs] of
          ".." -> MX.setElem '[' ls $ MX.setElem ']' rs $ MX.setElem '.' r $ MX.setElem '.' l mtx
          "]." -> moveBox (moveBox mtx (mv ls W, ls) d) (l, r) d
          ".[" -> moveBox (moveBox mtx (rs, mv rs E) d) (l, r) d
          "][" -> moveBox (moveBox (moveBox mtx (rs, mv rs E) d) (mv ls W, ls) d) (l, r) d
          "[]" -> moveBox (moveBox mtx (ls, rs) d) (l, r) d
          _ -> error $ "moveBox:" ++ show d
  W ->
    let w = mv l d
     in case mtx MX.! w of
          '.' -> MX.setElem '[' w $ MX.setElem ']' l $ MX.setElem '.' r mtx
          ']' -> moveBox (moveBox mtx (mv w W, w) d) (l, r) d
          _ -> error $ "moveBox:" ++ show d
  E ->
    let e = mv r d
     in case mtx MX.! e of
          '.' -> MX.setElem ']' e $ MX.setElem '[' r $ MX.setElem '.' l mtx
          '[' -> moveBox (moveBox mtx (e, mv e E) d) (l, r) d
          _ -> error $ "moveBox:" ++ show d

step :: Direction -> S.State (Coordinate, Plot) ()
step d = do
  (c, mtx) <- S.get
  let c' = mv c d
  when (canMove mtx c' d) $ do
    let plot' = move mtx c' d
    _ <- S.put (c', plot')
    return ()

step2 :: Direction -> S.State (Coordinate, Plot) ()
step2 d = do
  (c, mtx) <- S.get
  let c' = mv c d
  case mtx MX.! c' of
    '.' -> S.put (c', mtx)
    '#' -> return ()
    ']' -> let box = (mv c' W, c') in when (canMoveBox mtx box d) $ S.put (c', moveBox mtx box d)
    '[' -> let box = (c', mv c' E) in when (canMoveBox mtx box d) $ S.put (c', moveBox mtx box d)
    a -> error $ "step2: " ++ show a

parse :: String -> (Plot, [Direction])
parse text = (MX.fromLists (lines left), readDirection <$> filter (/= '\n') right)
  where
    [left, right] = splitOn "\n\n" text
    readDirection char = case char of
      '^' -> N
      'v' -> S
      '<' -> W
      '>' -> E
      s -> error $ show s

replace :: String -> String
replace = concatMap replace'
  where
    replace' c = case c of
      '#' -> "##"
      'O' -> "[]"
      '.' -> ".."
      '@' -> "@."
      _ -> [c]

gps :: Coordinate -> Int
gps (y, x) = (100 * pred y) + pred x

first :: String -> String
first input = show $ sum $ gps <$> coordinates
  where
    (mtx, steps) = parse input
    initial = maybe (0, 0) fst $ find (\(_, c) -> c == '@') $ MX.mapPos (,) mtx
    mtx' = MX.setElem '.' initial mtx
    (_, result) = S.execState (mapM step steps) (initial, mtx')
    coordinates = fst <$> filter (\(_, c) -> c == 'O') (MX.toList (MX.mapPos (,) result))

second :: String -> String
second input = show $ sum $ gps <$> coordinates
  where
    (mtx, steps) = parse $ replace input
    initial = maybe (0, 0) fst $ find (\(_, c) -> c == '@') $ MX.mapPos (,) mtx
    mtx' = MX.setElem '.' initial mtx
    (_, result) = S.execState (mapM step2 steps) (initial, mtx')
    coordinates = fst <$> filter (\(_, c) -> c == '[') (MX.toList (MX.mapPos (,) result))