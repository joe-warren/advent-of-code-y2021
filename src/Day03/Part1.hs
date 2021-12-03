{-# LANGUAGE DerivingVia #-}

module Day03.Part1 where

import Control.Exception (evaluate)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid (Ap (..), Sum (..))

newtype MonoidalMap k v = MonoidalMap {getMonoidalMap :: M.Map k v}

instance (Ord k, Semigroup v) => Semigroup (MonoidalMap k v) where
  (<>) a b = MonoidalMap $ M.unionWith (<>) (getMonoidalMap a) (getMonoidalMap b)

instance (Ord k, Monoid v) => Monoid (MonoidalMap k v) where
  mempty = MonoidalMap M.empty

newtype Average = Average (Int, Int) deriving (Semigroup, Monoid) via ((Sum Int, Sum Int))

newtype AverageRecord = AverageRecord {getMap :: M.Map Int Average} deriving (Semigroup, Monoid) via (MonoidalMap Int Average)

initialAverage :: Int -> Average
initialAverage i = Average (i, 1)

foldMapMWithIndex :: (Foldable t, Monoid b, Monad m) => (Int -> a -> m b) -> t a -> m b
foldMapMWithIndex f = getAp . foldMap (Ap . uncurry f) . zip [0 ..] . toList

parseChar :: Char -> Either String Int
parseChar '1' = pure 1
parseChar '0' = pure 0
parseChar x = Left $ "unrecognised value : " ++ [x]

parseLine :: String -> Either String AverageRecord
parseLine = foldMapMWithIndex (\i c -> AverageRecord . M.singleton i . initialAverage <$> parseChar c) . reverse

evaluateRecord :: AverageRecord -> (Int, Int)
evaluateRecord =
  let evalOne (i, Average (total, n)) = let v = 2 ^ i in if total > n `div` 2 then (Sum v, Sum 0) else (Sum 0, Sum v)
   in bimap getSum getSum . foldMap evalOne . M.toList . getMap

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/day3.txt"
  let f = getAp $ foldMap (Ap . parseLine) l
  either putStrLn ((print <> print . uncurry (*)) . evaluateRecord) f