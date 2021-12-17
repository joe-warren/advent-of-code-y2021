{-# LANGUAGE OverloadedStrings #-}

module Day17.Part1 where

import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

type Parser a = MP.Parsec Void String a

data Window a = Window {bottomLeft :: (a, a), topRight :: (a, a)} deriving (Eq, Show)

parser :: Parser (Window Int)
parser = do
  let num = MP.signed (pure ()) MP.decimal
  _ <- "target area: x="
  x1 <- num
  _ <- ".."
  x2 <- num
  _ <- ", y="
  y1 <- num
  _ <- ".."
  y2 <- num
  let xLo = min x1 x2
  let xHi = max x1 x2
  let yLo = min y1 y2
  let yHi = max y1 y2
  pure $ Window (xLo, yLo) (xHi, yHi)

data Probe a = Probe {probePosition :: (a, a), probeVelocity :: (a, a)} deriving (Show, Eq)

(^+^) :: Num a => (a, a) -> (a, a) -> (a, a)
(x1, y1) ^+^ (x2, y2) = (x1 + x2, y1 + y2)

probeStep :: Num a => Probe a -> Probe a
probeStep (Probe pos vel@(vx, vy)) = Probe (pos ^+^ vel) (vx - signum vx, vy -1)

within :: Ord a => Probe a -> Window a -> Bool
within (Probe (x, y) _) (Window (xLo, yLo) (xHi, yHi)) = and [xLo <= x, x <= xHi, yLo <= y, y <= yHi]

hasPassed :: (Num a, Ord a) => Probe a -> Window a -> Bool
hasPassed (Probe (x, y) (_, vy)) (Window (_, yLo) (xHi, _)) = (vy < 0 && y < yLo) || x > xHi

testHit :: (Num a, Ord a) => Window a -> Probe a -> Maybe a
testHit window probe
  | probe `within` window = pure . snd . probePosition $ probe
  | probe `hasPassed` window = Nothing
  | otherwise = max (snd . probePosition $ probe) <$> testHit window (probeStep probe)

initialProbe :: Int -> Int -> Probe Int
initialProbe dx dy = Probe (0, 0) (dx, dy)

findBestHeight :: Window Int -> Int
findBestHeight window = length $ filter isJust $ testHit window <$> (initialProbe <$> [1 .. 1000] <*> [-1000 .. 1000])

main :: IO ()
main = do
  contents <- readFile "inputs/day17.txt"
  case MP.parse parser "day17" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right window -> do
      print window
      print $ findBestHeight window