{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day08.Part1 where

import Control.Applicative (many)
import Control.Arrow ((&&&))
import Control.Monad (void)
import Data.Bool
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid (Endo (Endo, appEndo))
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

segments :: Int -> [Int]
segments 0 = [0, 1, 2, 3, 4, 5]
segments 1 = [1, 2]
segments 2 = [0, 1, 3, 4, 6]
segments 3 = [0, 1, 2, 3, 6]
segments 4 = [1, 2, 5, 6]
segments 5 = [0, 2, 3, 5, 6]
segments 6 = [0, 2, 3, 4, 5, 6]
segments 7 = [0, 1, 2]
segments 8 = [0, 1, 2, 3, 4, 5, 6]
segments 9 = [0, 1, 2, 3, 5, 6]
segments _ = undefined

initialCandidates :: Map Char [Int]
initialCandidates = M.fromList [(c, [0 .. 7]) | c <- ['a' .. 'g']]

mapWhere :: (k -> Bool) -> (v -> v) -> Map k v -> Map k v
mapWhere f' f = M.mapWithKey (\k v -> if f' k then f v else v)

solve :: Line -> Map Char [Int]
solve (elems, final) = appEndo (foldMap (Endo . step) (elems <> tuple4ToList final)) initialCandidates
  where
    knownLengths = [2, 3, 4, 7]
    byLengths = M.fromListWith (++) $ ((length . segments) &&& segments) <$> [1 .. 9]
    step :: Set Char -> Map Char [Int] -> Map Char [Int]
    step s m =
      let segs = fromJust $ length s `M.lookup` byLengths
       in mapWhere (\x -> (not (x `elem` s)) && (length s `elem` knownLengths)) (filter (not . (`elem` segs)))
            . mapWhere (`elem` s) (filter (`elem` segs))
            $ m

tryScoreOne :: Set Char -> Map Char [Int] -> Maybe Int
tryScoreOne letters solution = case filter f' [0 .. 9] of
  [v] -> Just v
  _ -> Nothing
  where
    f' v =
      let s = segments v
          f'' l =
            if l `elem` letters
              then any (`elem` s) (fromJust $ M.lookup l solution)
              else not $ all (`elem` s) (fromJust $ M.lookup l solution)
       in all f'' ['a' .. 'g'] && length s == length letters

tryScore :: Line -> Map Char [Int] -> Maybe Int
tryScore (_, (a, b, c, d)) sol =
  sum
    <$> sequence
      [ (1000 *) <$> tryScoreOne a sol,
        (100 *) <$> tryScoreOne b sol,
        (10 *) <$> tryScoreOne c sol,
        (1 *) <$> tryScoreOne d sol
      ]

runLine :: Line -> Maybe Int
runLine l = tryScore l (solve l)

main :: IO ()
main = do
  contents <- T.readFile "inputs/day8.txt"
  case MP.parse parser "day8" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right nums -> do
      print $ sum <$> traverse runLine nums