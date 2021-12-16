{-# LANGUAGE TupleSections #-}

module Day15.Part1 where

import Data.Bifunctor (bimap)
import qualified Data.Char as Char
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List.Extra (minimumOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

type Parser a = MP.Parsec Void Text a

parser :: Parser (Grid Int)
parser = Grid . V.fromList <$> MP.sepEndBy (V.fromList <$> MP.some (Char.digitToInt <$> MP.digitChar)) MP.newline

newtype Grid a = Grid {unGrid :: Vector (Vector a)} deriving (Show)

gridSize :: Grid a -> (Int, Int)
gridSize (Grid g) = (length g, maybe 0 length $ listToMaybe . V.toList $ g)

(!?) :: Grid a -> (Int, Int) -> Maybe a
(Grid g) !? (x, y) = (g V.!? x) >>= (V.!? y)

score :: Grid Int -> [(Int, Int)] -> Int
score g inds = maybe 0 sum $ traverse (g !?) inds

data StateValue = StateValue {visited :: Bool, currentDistance :: Int, parent :: (Int, Int)}

neighbourCoords :: (Int, Int) -> [(Int, Int)]
neighbourCoords (x, y) = [(x + 1, y), (x, y + 1), (x -1, y), (x, y -1)]

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn f x y = if f x < f y then x else y

djikstra :: Grid Int -> [(Int, Int)]
djikstra g = traceBack $ buildShortest (0, 0) (M.singleton (0, 0) (StateValue False 0 (0, 0)))
  where
    dest = bimap (subtract 1) (subtract 1) (gridSize g)
    buildShortest :: (Int, Int) -> Map (Int, Int) StateValue -> Map (Int, Int) StateValue
    buildShortest cur state
      | cur == dest = state
      | otherwise =
        let neighbours = mapMaybe (\c -> (c,) <$> (g !? c)) $ neighbourCoords cur
            curDist = currentDistance $ fromJust (M.lookup cur state)
            neighbourMap = M.fromList $ fmap (\(c, v) -> (c, StateValue False (curDist + v) cur)) neighbours
            updatedMap = M.alter (fmap (\x -> x {visited = True})) cur $ M.unionWith (minOn currentDistance) state neighbourMap

            nextLink = fst $ minimumOn (currentDistance . snd) $ M.toList $ M.filter (not . visited) updatedMap
         in buildShortest nextLink updatedMap
    traceBack :: Map (Int, Int) StateValue -> [(Int, Int)]
    traceBack m = go dest
      where
        go s = case parent <$> M.lookup s m of
          Nothing -> error "wat"
          Just (0, 0) -> [s]
          Just other -> s : go other

main :: IO ()
main = do
  contents <- T.readFile "inputs/day15.txt"
  case MP.parse parser "day15" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right nums -> do
      print nums
      print $ score nums $ djikstra nums