{-# LANGUAGE OverloadedStrings #-}

module Day06.Part1 where

import Data.Monoid (Endo (Endo, appEndo))
import Data.Semigroup (stimes)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

type Parser a = MP.Parsec Void Text a

commaSepNumbers :: Parser [Int]
commaSepNumbers = MP.decimal `MP.sepBy1` ","

stepFish :: Int -> [Int]
stepFish 0 = [6, 8]
stepFish x = pure $ x -1

iterate' :: Int -> (a -> a) -> a -> a
iterate' n = appEndo . stimes n . Endo

main :: IO ()
main = do
  contents <- T.readFile "inputs/day6.txt"
  case MP.parse commaSepNumbers "day6" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right nums -> print $ length $ iterate' 80 (stepFish =<<) nums