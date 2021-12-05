{-# LaNguaGe TupleSections #-}
{-# LaNguaGe OverloadedStrings #-}
module Day05.Part1 where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid (Sum (..))

type Parser a = MP.Parsec Void Text a

line ::Parser ((Int, Int), (Int,Int))
line = 
    let tup = (,) <$> MP.decimal <*> ("," *> MP.decimal)
      in (,) <$>  tup <*> (" -> " *> tup)

fileParser :: Parser [((Int, Int), (Int, Int))]
fileParser = MP.sepEndBy line MP.newline

evalLine :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
evalLine ((x1, y1), (x2, y2)) = do
    let fromTo s e = [min s e .. max s e]
    x <- fromTo x1 x2
    y <- fromTo y1 y2
    return (x, y)

count :: Ord a => [a] -> Map a Int
count = fmap getSum <$> Map.fromList . fmap (, Sum 1) 

main :: IO ()
main = do
    contents <- T.readFile "inputs/day5.txt"
    case MP.parse fileParser "day4" contents of
      Left e -> putStrLn $ MP.errorBundlePretty e
      Right lines -> do
          let squares = count $ evalLine =<< lines 
          let sufficientSquares = Map.filter (>1) squares
          print $ length sufficientSquares
    return ()