{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Day11.Part1 where

import Control.Arrow (Kleisli (Kleisli, runKleisli))
import Control.Monad.Identity (Identity (Identity, runIdentity))
import Control.Monad.Writer
import qualified Data.Char as Char
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Semigroup (stimes)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

type Parser a = MP.Parsec Void Text a

newtype Grid a = Grid {unGrid :: Vector (Vector a)} deriving (Functor)

(!?) :: Grid a -> (Int, Int) -> Maybe a
(Grid g) !? (x, y) = (g V.!? x) >>= (V.!? y)

imapM :: Monad m => ((Int, Int) -> a -> m b) -> Grid a -> m (Grid b)
imapM f (Grid d) = Grid <$> V.imapM (V.imapM . (f .) . (,)) d

imap :: ((Int, Int) -> a -> b) -> Grid a -> Grid b
imap f = runIdentity . imapM ((Identity .) . f)

parser :: Parser (Grid Int)
parser = Grid . V.fromList <$> MP.sepBy (V.fromList <$> MP.many (Char.digitToInt <$> MP.digitChar)) MP.newline

incrementOne :: (Int, Int) -> Int -> Writer (Sum Int, [(Int, Int)]) Int
incrementOne pos v
  | v > 9 = 0 <$ tell (Sum 1, pure pos)
  | otherwise = pure v

incrementExplodables :: Grid Int -> Writer (Sum Int) ([(Int, Int)], Grid Int)
incrementExplodables g = mapWriter (\(a, (count, pos)) -> ((pos, a), count)) $ imapM incrementOne g

neighbourCoords :: (Int, Int) -> [(Int, Int)]
neighbourCoords (x, y) = filter (/= (x, y)) $ do
  let steps = [(+ 1), id, subtract 1]
  f <- steps
  g <- steps
  return (f x, g y)

incrementIfNot0 :: Int -> Int
incrementIfNot0 0 = 0
incrementIfNot0 i = i + 1

doExplode :: Grid Int -> Writer (Sum Int) (Grid Int)
doExplode g = do
  (explosionPositions, updatedG) <- incrementExplodables g
  case explosionPositions of
    [] -> pure updatedG
    _ -> do
      let toIncrement = M.fromListWith (+) $ (,1) <$> (neighbourCoords =<< explosionPositions)
      let doubleUpdatedG = imap (\pos v -> if v == 0 then 0 else v + fromMaybe 0 (M.lookup pos toIncrement)) updatedG
      doExplode doubleUpdatedG

step :: Grid Int -> Writer (Sum Int) (Grid Int)
step = doExplode . fmap (+ 1)

repeatM :: Monad m => Int -> (a -> m a) -> a -> m a
repeatM n f = (appEndo $ stimes n (Endo (f =<<))) . pure

score :: Grid Int -> Int
score = getSum . execWriter . repeatM 100 step

main :: IO ()
main = do
  contents <- T.readFile "inputs/day11.txt"
  case MP.parse parser "day11" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right nums -> print $ score nums