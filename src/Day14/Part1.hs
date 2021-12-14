{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day14.Part1 where

import Control.Applicative (many)
import Data.Bits (Bits (xor))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup (Endo (Endo), appEndo, stimes)
import Data.Void (Void)
import Text.Megaparsec (getOffset)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

type Parser a = MP.Parsec Void String a

parser :: Parser (String, Map (Char, Char) Char)
parser =
  let mappingParser = (,) <$> (((,) <$> MP.upperChar <*> MP.upperChar) <* " -> ") <*> MP.upperChar
      mapParser = M.fromList <$> MP.sepEndBy mappingParser MP.newline
   in (,) <$> (many MP.upperChar <* MP.newline <* MP.newline) <*> mapParser

doSubstitution :: Map (Char, Char) Char -> String -> String
doSubstitution map = go
  where
    go (a : b : xs) = case M.lookup (a, b) map of
      Just v -> a : v : (go (b : xs))
      Nothing -> a : go (b : xs)
    go [x] = pure x
    go [] = []

repeatedly :: Int -> (a -> a) -> a -> a
repeatedly n = appEndo . stimes n . Endo

score :: (Eq a, Ord a) => [a] -> Int
score l =
  let m = M.fromListWith (+) $ (,1) <$> l
   in maximum m - minimum m

main :: IO ()
main = do
  contents <- readFile "inputs/day14.txt"
  case MP.parse parser "day14" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right (string, map) -> do
      print $ score $ repeatedly 10 (doSubstitution map) string