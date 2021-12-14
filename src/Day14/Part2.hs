{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day14.Part1 where

import Control.Applicative (many)
import qualified Control.Monad.RWS.Class as M
import Data.Bits (Bits (xor))
import Data.Foldable (Foldable (foldl'), toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Semigroup (Endo (Endo), appEndo, stimes)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec (getOffset)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

type Parser a = MP.Parsec Void String a

parser :: Parser (String, Map (Char, Char) Char)
parser =
  let mappingParser = (,) <$> (((,) <$> MP.upperChar <*> MP.upperChar) <* " -> ") <*> MP.upperChar
      mapParser = M.fromList <$> MP.sepEndBy mappingParser MP.newline
   in (,) <$> (many MP.upperChar <* MP.newline <* MP.newline) <*> mapParser

unionMaps :: (Ord k, Num v) => Map k v -> Map k v -> Map k v
unionMaps = M.unionWith (+)

doSubstitution :: Int -> Map (Char, Char) Char -> String -> Map Char Int
doSubstitution n map s =
  let allChars = S.fromList s <> S.fromList (toList map) <> S.fromList (concat $ flattenTuple <$> M.keys map)
      allArgs = S.fromList $ do
        f <- toList allChars
        s <- toList allChars
        k <- [0 .. n]
        return (f, s, k)
      go (f, s, 0) = M.singleton f 1
      go (f, s, k) = case M.lookup (f, s) map of
        Nothing -> M.singleton f 1
        Just m -> go' (f, m, k -1) `unionMaps` go' (m, s, k -1)
      go' = fromMaybe M.empty . (`M.lookup` memo)
      memo = M.fromSet go allArgs
      allPairs = zip s (tail s)
      combine m (a, b) = m `unionMaps` (fromMaybe M.empty (M.lookup (a, b, n) memo))
   in M.alter (fmap (+ 1)) (last s) (foldl' combine M.empty allPairs)

score :: Foldable f => f Int -> Int
score m = maximum m - minimum m

flattenTuple :: (a, a) -> [a]
flattenTuple (a, b) = [a, b]

main :: IO ()
main = do
  contents <- readFile "inputs/day14.txt"
  case MP.parse parser "day14" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right (string, map) -> do
      print $ score $ doSubstitution 40 map string

--print $ score $ repeatedly 10 (doSubstitution map) string