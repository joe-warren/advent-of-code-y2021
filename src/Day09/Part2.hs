{-# LANGUAGE TupleSections #-}

module Day09.Part1 where

import Control.Monad (guard)
import qualified Data.Char as Char
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (minimumBy, sort, sortBy, transpose)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

newtype Grid a = Grid {unGrid :: Vector (Vector a)}

gridSize :: Grid a -> (Int, Int)
gridSize (Grid g) = (length g, maybe 0 length $ listToMaybe . V.toList $ g)

(!?) :: Grid a -> (Int, Int) -> Maybe a
(Grid g) !? (x, y) = (g V.!? x) >>= (V.!? y)

type Parser a = MP.Parsec Void Text a

neighbourCoords :: (Int, Int) -> [(Int, Int)]
neighbourCoords (x, y) = [(x + 1, y), (x, y + 1), (x -1, y), (x, y -1)]

regions :: Grid Int -> Map (Int, Int) [(Int, Int)]
regions g =
  let (w, h) = gridSize g
      loop p = case g !? p of
        Nothing -> p
        Just 9 -> p
        Just v ->
          let neighbours = neighbourCoords p
              neighbourCandidates = fmap snd . sortBy (compare `on` fst) . filter ((< v) . fst) $ catMaybes $ (\x -> (,x) <$> (g !? x)) <$> neighbours
           in case neighbourCandidates of
                v : _ -> loop v
                [] -> p
   in M.fromListWith (<>) [(loop (x, y), [(x, y)]) | x <- [0 .. w], y <- [0 .. h]]

score :: Map a [b] -> Int
score m = case reverse . sort . toList $ length <$> m of
  a : b : c : _ -> a * b * c
  _ -> 0

parser :: Parser (Grid Int)
parser = Grid . V.fromList <$> MP.sepBy (V.fromList <$> MP.many (Char.digitToInt <$> MP.digitChar)) MP.newline

main :: IO ()
main = do
  contents <- T.readFile "inputs/day9.txt"
  case MP.parse parser "day9" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right nums -> print $ score $ regions nums