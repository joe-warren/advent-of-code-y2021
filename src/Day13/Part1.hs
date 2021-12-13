{-# LANGUAGE OverloadedStrings #-}

module Day13.Part1 where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

type Parser a = MP.Parsec Void Text a

data Axis = AxisX | AxisY deriving (Show)

data Fold = Fold Axis Int deriving (Show)

tupleParser :: Parser (Int, Int)
tupleParser = (,) <$> (MP.decimal <* ",") <*> MP.decimal

foldParser :: Parser Fold
foldParser =
  let axisParser = (AxisX <$ "x") <|> (AxisY <$ "y")
   in Fold <$> ("fold along " *> axisParser) <*> ("=" *> MP.decimal)

parser :: Parser (Set (Int, Int), [Fold])
parser = (,) <$> (S.fromList <$> MP.sepEndBy tupleParser MP.newline <* MP.newline) <*> MP.sepEndBy foldParser MP.newline

axisTuple :: Axis -> (a -> a) -> (a, a) -> (a, a)
axisTuple AxisX = first
axisTuple AxisY = second

applyFoldOnce :: Fold -> (Int, Int) -> (Int, Int)
applyFoldOnce (Fold axis mid) =
  let f x = if x < mid then x else 2 * mid - x
   in axisTuple axis f

applyFold :: Fold -> Set (Int, Int) -> Set (Int, Int)
applyFold f = S.map (applyFoldOnce f)

main :: IO ()
main = do
  contents <- T.readFile "inputs/day13.txt"
  case MP.parse parser "day13" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right (dots, f : folds) -> do
      print dots
      print folds
      print $ length $ applyFold f dots
    _ -> print "no folds"