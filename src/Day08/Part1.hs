{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day08.Part1 where

import Control.Applicative (many)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

type Parser a = MP.Parsec Void Text a

type Line = ([Set Char], (Set Char, Set Char, Set Char, Set Char))

lineParser :: Parser Line
lineParser =
  let set = (S.fromList <$> many MP.alphaNumChar)
   in (,) <$> (set `MP.endBy` " ") <* "| " <*> ((,,,) <$> set <* " " <*> set <* " " <*> set <* " " <*> set)

parser :: Parser [Line]
parser = lineParser `MP.endBy` MP.newline

tuple4ToList :: (a, a, a, a) -> [a]
tuple4ToList (x, y, z, w) = [x, y, z, w]

score :: [Line] -> Int
score xs = length . filter (`elem` [2, 3, 4, 7]) $ length <$> concat (tuple4ToList . snd <$> xs)

main :: IO ()
main = do
  contents <- T.readFile "inputs/day8.txt"
  case MP.parse parser "day8" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right nums -> print $ score nums