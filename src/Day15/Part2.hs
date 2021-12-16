{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Day15.Part2 where

import Control.Arrow (Arrow ((&&&)), (***))
import Control.Monad.Identity (Identity (Identity, runIdentity))
import Data.Bifunctor (bimap)
import qualified Data.Char as Char
import Data.Foldable (minimumBy, toList)
import Data.Function (on)
import Data.HashMap (Map)
import qualified Data.HashMap as M
import Data.List.Extra (minimumOn)
import Data.Maybe (fromJust, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
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

newtype Grid a = Grid {unGrid :: Vector (Vector a)} deriving (Show, Functor)

gridSize :: Grid a -> (Int, Int)
gridSize (Grid g) = (length g, maybe 0 length $ listToMaybe . V.toList $ g)

(!?) :: Grid a -> (Int, Int) -> Maybe a
(Grid g) !? (x, y) = (g V.!? x) >>= (V.!? y)

data StateValue = StateValue {notVisited :: Bool, currentDistance :: Int, parent :: (Int, Int)}

neighbourCoords :: (Int, Int) -> [(Int, Int)]
neighbourCoords (x, y) = [(x + 1, y), (x, y + 1), (x -1, y), (x, y -1)]

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn f x y = if f x < f y then x else y

djikstra :: Grid Int -> Int
djikstra g = buildShortest (0, 0) 0 (M.singleton (0, 0) (StateValue True 0 (0, 0)))
  where
    dest = bimap (subtract 1) (subtract 1) (gridSize g)
    buildShortest :: (Int, Int) -> Int -> Map (Int, Int) StateValue -> Int
    buildShortest cur curDist state
      | cur == dest = curDist
      | otherwise =
        let neighbours = mapMaybe (\c -> (c,) <$> (g !? c)) $ neighbourCoords cur
            neighbourMap = M.fromList $ fmap (\(c, v) -> (c, StateValue True (curDist + v) cur)) neighbours
            updatedMap = M.alter (fmap (\x -> x {notVisited = False})) cur $ M.unionWith (minOn (notVisited &&& currentDistance)) state neighbourMap

            (nextLink, s) = minimumOn (currentDistance . snd) $ M.toList $ M.filter notVisited updatedMap
         in buildShortest nextLink (currentDistance s) updatedMap
    traceBack :: Map (Int, Int) StateValue -> [(Int, Int)]
    traceBack m = go dest
      where
        go s = case parent <$> M.lookup s m of
          Nothing -> error "wat"
          Just (0, 0) -> [s]
          Just other -> s : go other

imapM :: Monad m => ((Int, Int) -> a -> m b) -> Grid a -> m (Grid b)
imapM f (Grid d) = Grid <$> V.imapM (V.imapM . (f .) . (,)) d

imap :: ((Int, Int) -> a -> b) -> Grid a -> Grid b
imap f = runIdentity . imapM ((Identity .) . f)

scale :: Int -> Grid Int -> Grid Int
scale n g@(Grid d) =
  let (w, h) = gridSize g
   in imap (\(x, y) v -> ((v + (x `div` w) + (y `div` h) -1) `mod` 9) + 1) $ Grid $ (V.concat . (replicate n)) <$> V.concat (replicate n d)

toString :: Show a => Grid a -> Text
toString g = T.unlines . toList . fmap (T.concat . toList) . unGrid $ T.pack . show <$> g

main :: IO ()
main = do
  contents <- T.readFile "inputs/day15.txt"
  case MP.parse parser "day15" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right nums -> do
      let giantNums = scale 3 nums
      --T.putStrLn $ toString giantNums
      print $ djikstra giantNums