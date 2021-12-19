{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Day18.Part2 where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Foldable (traverse_)
import Data.List (foldl1')
import Data.Maybe (fromMaybe)
import Data.Text (Text, scanl)
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

type Parser a = MP.Parsec Void String a

data Tree a = Leaf a | Pair (Tree a) (Tree a) deriving (Functor, Foldable, Traversable, Show, Eq)

data Zipper a = Zipper {path :: [Either (Tree a) (Tree a)], value :: Tree a}

treeParser :: Parser (Tree Int)
treeParser =
  let leaf = Leaf <$> MP.decimal
      pair = MP.between "[" "]" $ Pair <$> (treeParser <* ",") <*> treeParser
   in leaf <|> pair

parser :: Parser [Tree Int]
parser = MP.sepEndBy treeParser MP.newline

addLeft :: Num i => i -> Tree i -> Tree i
addLeft v (Pair l r) = Pair (addLeft v l) r
addLeft v (Leaf l) = Leaf (v + l)

addRight :: Num i => i -> Tree i -> Tree i
addRight v (Pair l r) = Pair l (addRight v r)
addRight v (Leaf l) = Leaf (v + l)

leftmost :: Tree i -> i
leftmost (Leaf i) = i
leftmost (Pair l _) = leftmost l

rightmost :: Tree i -> i
rightmost (Leaf i) = i
rightmost (Pair _ r) = rightmost r

explode :: Num i => Int -> Tree i -> Maybe (i, Tree i, i)
explode depth (Pair (Leaf l) (Leaf r))
  | depth >= 4 = Just (l, Leaf 0, r)
explode depth (Pair l r) =
  (handleLeftExplode <$> explode (depth + 1) l) <|> (handleRightExplode <$> explode (depth + 1) r)
  where
    handleLeftExplode (vl, t, vr) = (vl, Pair t $ addLeft vr r, 0)
    handleRightExplode (vl, t, vr) = (0, Pair (addRight vl l) t, vr)
explode _ (Leaf a) = Nothing

split :: Integral i => Tree i -> Maybe (Tree i)
split (Leaf l) = if l >= 10 then Just (Pair (Leaf $ l `div` 2) (Leaf $ l - l `div` 2)) else Nothing
split (Pair l r) = ((`Pair` r) <$> split l) <|> (Pair l <$> split r)

reduce :: Integral i => Tree i -> Tree i
reduce t = case explode 0 t of
  Just (_, t', _) -> reduce t'
  Nothing -> maybe t reduce (split t)

add :: Integral i => Tree i -> Tree i -> Tree i
add a b = reduce $ Pair a b

magnitude :: Num p => Tree p -> p
magnitude (Pair l r) = 3 * (magnitude l) + 2 * (magnitude r)
magnitude (Leaf l) = l

highest :: [Tree Int] -> Int
highest ts = maximum $ do
  a <- ts
  b <- ts
  guard $ a /= b
  return $ magnitude $ add a b

main :: IO ()
main = do
  contents <- readFile "inputs/day18.txt"
  case MP.parse parser "day18" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right trees -> do
      traverse_ print trees
      putStrLn ""
      print $ highest trees
