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

mean :: [Int] -> Float
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

{--
score :: [Int] -> Int
score xs =
  let scoreOne m = sum $ (`div` 2) . (\x -> (x + 1) * x) . abs . subtract m <$> xs
   in minimum $ scoreOne <$> [minimum xs .. maximum xs]
--}
score :: [Int] -> Int
score xs =
  let scoreOne m = sum $ (`div` 2) . (\x -> (x + 1) * x) . abs . subtract m <$> xs
      meanVal = mean xs
   in min (scoreOne (floor meanVal)) (scoreOne (ceiling meanVal))

main :: IO ()
main = do
  contents <- T.readFile "inputs/day7.txt"
  case MP.parse commaSepNumbers "day7" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right nums -> print $ score nums