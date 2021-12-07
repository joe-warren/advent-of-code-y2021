{-# LANGUAGE OverloadedStrings #-}

module Day07.Part1 where

import Data.List (sort)
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

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)

score :: [Int] -> Int
score xs = let m = median xs in sum $ abs . subtract m <$> xs

main :: IO ()
main = do
  contents <- T.readFile "inputs/day7.txt"
  case MP.parse commaSepNumbers "day7" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right nums -> print $ score nums