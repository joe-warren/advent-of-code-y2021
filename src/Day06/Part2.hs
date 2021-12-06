{-# LANGUAGE OverloadedStrings #-}

module Day06.Part2 where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
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

intSet :: IS.IntSet
intSet = IS.fromList [0 .. 256]

intMap :: IM.IntMap Int
intMap = IM.fromSet go intSet
  where
    go i
      | i < 7 = 2
      | otherwise = (intMap IM.! (i - 7)) + (if i < 9 then 1 else (intMap IM.! (i - 9)))

fishChildren :: Int -> Int -> Int
fishChildren daysLeft i = intMap IM.! (daysLeft - i - 1)

main :: IO ()
main = do
  contents <- T.readFile "inputs/day6.txt"
  case MP.parse commaSepNumbers "day6" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right nums -> print $ sum $ fishChildren 256 <$> nums