module Day3 (first, second) where

import Data.Either (fromRight)
import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec as PC

expressionParser :: PC.Parser (Int, Int)
expressionParser = do
  _ <- P.string "mul"
  _ <- P.char '('
  left <- P.many1 P.digit
  _ <- P.char ','
  right <- P.many1 P.digit
  _ <- P.char ')'
  return (read left, read right)

doParser :: PC.Parser Bool
doParser = P.string "do()" >> return True

dontParser :: PC.Parser Bool
dontParser = P.string "don't()" >> return False

expressions :: PC.Parser [(Int, Int)]
expressions = loop
  where
    loop =
      (P.eof >> return [])
        P.<|> do r <- P.try expressionParser; rs <- loop; return (r : rs)
        P.<|> (P.anyChar >> loop)

extractEnabled :: PC.Parser String
extractEnabled = loop True
  where
    loop state =
      (P.eof >> return "")
        P.<|> do r <- P.try doParser P.<|> dontParser; loop r
        P.<|> do c <- P.anyChar; rs <- loop state; return $ if state then c : rs else rs

parse :: String -> [(Int, Int)]
parse text = fromRight (error "no-op") res
  where
    res = P.parse expressions "expressionParser" text

parse2 :: String -> [(Int, Int)]
parse2 text = fromRight (error "no-op") $ P.parse expressions "expressionParser" res
  where
    res = fromRight (error "no-op") $ P.parse extractEnabled "doParser" text

first :: String -> String
first input = show $ sum multiples
  where
    multiples = uncurry (*) <$> parse input

second :: String -> String
second input = show $ sum multiples
  where
    multiples = uncurry (*) <$> parse2 input