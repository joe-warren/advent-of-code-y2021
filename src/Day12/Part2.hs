{-# LANGUAGE OverloadedStrings #-}

module Day12.Part2 where

import Control.Applicative (Alternative (some, (<|>)), many)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

type Parser a = MP.Parsec Void Text a

data Cave = Start | End | Big Text | Small Text deriving (Eq, Ord, Show)

caveParser :: Parser Cave
caveParser = (Start <$ "start") <|> (End <$ "end") <|> (Big . T.pack <$> some MP.upperChar) <|> (Small . T.pack <$> some MP.lowerChar)

parser :: Parser (Map Cave [Cave])
parser =
  let line = (,) <$> caveParser <* "-" <*> (caveParser)
      dup (a, b) = [(a, [b]), (b, [a])]
   in M.fromListWith (<>) . concatMap dup <$> MP.sepEndBy line MP.newline

pathsToEnd :: Map Cave [Cave] -> [[Cave]]
pathsToEnd map = go False [] Start
  where
    go _ _ End = pure [End]
    go visitedTwice visited node = do
      (newVisitedTwice, newVisited) <- case node of
        Small t ->
          if t `elem` visited
            then (if visitedTwice then [] else pure (True, visited))
            else pure $ (visitedTwice, t : visited)
        _ -> pure (visitedTwice, visited)
      next <- filter (/= Start) $ fromMaybe [] $ M.lookup node map
      (node :) <$> go newVisitedTwice newVisited next

main :: IO ()
main = do
  contents <- T.readFile "inputs/day12.txt"
  case MP.parse parser "day12" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right graph -> print $ length $ pathsToEnd graph