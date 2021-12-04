{-# LANGUAGE DerivingVia #-}

module Day03.Part2 where

import Control.Exception (evaluate)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap (..), Sum (..))

newtype MonoidalMap k v = MonoidalMap {getMonoidalMap :: M.Map k v}

instance (Ord k, Semigroup v) => Semigroup (MonoidalMap k v) where
  (<>) a b = MonoidalMap $ M.unionWith (<>) (getMonoidalMap a) (getMonoidalMap b)

instance (Ord k, Monoid v) => Monoid (MonoidalMap k v) where
  mempty = MonoidalMap M.empty

newtype Average = Average (Int, Int)
  deriving (Show)
  deriving (Semigroup, Monoid) via ((Sum Int, Sum Int))

newtype AverageRecord = AverageRecord {getMap :: M.Map Int Average}
  deriving (Show)
  deriving (Semigroup, Monoid) via (MonoidalMap Int Average)

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

evalAverage :: Average -> Int
evalAverage (Average (total, n)) = bool 0 1 $ total * 2 >= n

evaluateRecord :: AverageRecord -> Int
evaluateRecord =
  let evalOne (i, a) = Sum $ (2 ^ i) * evalAverage a
   in getSum . foldMap evalOne . M.toList . getMap

evaluateRecords :: Int -> [AverageRecord] -> (Int -> Int -> Bool) -> Maybe AverageRecord
evaluateRecords n records keep =
  if n < 0
    then Nothing
    else
      let mode = mconcat records
       in case filter
            ( \v ->
                let mode' = evalAverage <$> M.lookup n (getMap $ mode)
                    val = evalAverage <$> M.lookup n (getMap $ v)
                 in fromMaybe True (keep <$> mode' <*> val)
            )
            records of
            [one] -> pure one
            [] -> Nothing
            many -> evaluateRecords (n -1) many keep

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/day3.txt"
  let f = traverse parseLine l
  case f of
    Left e -> print e
    Right records -> do
      oxygen <- maybe (error "no oxygen") pure $ evaluateRecords 13 records (==)
      carbon <- maybe (error "no carbon") pure $ evaluateRecords 13 records (/=)
      (print <> print . uncurry (*)) (evaluateRecord oxygen, evaluateRecord carbon)
