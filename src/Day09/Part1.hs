module Day09.Part1 where

import Control.Monad (guard)
import qualified Data.Char as Char
import Data.Functor (($>))
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

type Parser a = MP.Parsec Void Text a

parser :: Parser [[Int]]
parser = MP.sepBy (MP.many (Char.digitToInt <$> MP.digitChar)) MP.newline

isLowest :: Ord a => Maybe a -> a -> Maybe a -> Bool
isLowest a b c = maybe True (> b) a && maybe True (> b) c

convolveLine :: (Maybe a -> a -> Maybe a -> b) -> [a] -> [b]
convolveLine f xs = zipWith3 f (Nothing : (pure <$> xs)) xs ((pure <$> tail xs) ++ [Nothing])

zipWith2d :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWith2d f = zipWith (zipWith f)

selectPoints :: [[Bool]] -> [[a]] -> [a]
selectPoints toSelect points = concatMap catMaybes $ zipWith2d (($>) . guard) toSelect points

lowPoints :: [[Int]] -> [Int]
lowPoints points =
  let lowestHorizontally = convolveLine isLowest <$> points
      lowestVertically = transpose $ convolveLine isLowest <$> transpose points
      lowestGenerally = zipWith2d (&&) lowestHorizontally lowestVertically
   in selectPoints lowestGenerally points

riskLevel :: Int -> Int
riskLevel = (+ 1)

main :: IO ()
main = do
  contents <- T.readFile "inputs/day9.txt"
  case MP.parse parser "day9" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right nums -> do print $ sum $ riskLevel <$> lowPoints nums