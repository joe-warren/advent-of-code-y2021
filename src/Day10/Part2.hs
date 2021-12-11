module Day10.Part1 where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe)
import Data.Monoid (Dual (Dual, getDual), Endo (Endo, appEndo))

data Brace = Squigly | Round | Square | Ancle deriving (Eq, Show)

data Direction = Open | Close deriving (Eq, Show)

data Bracket = Bracket {bracketKind :: Brace, bracketDirection :: Direction}

data Line = Corrupted Brace | Incomplete (NonEmpty Brace) | Complete

parseChar :: Char -> Either Char Bracket
parseChar '[' = pure $ Bracket Square Open
parseChar '(' = pure $ Bracket Round Open
parseChar '{' = pure $ Bracket Squigly Open
parseChar '<' = pure $ Bracket Ancle Open
parseChar ']' = pure $ Bracket Square Close
parseChar ')' = pure $ Bracket Round Close
parseChar '}' = pure $ Bracket Squigly Close
parseChar '>' = pure $ Bracket Ancle Close
parseChar other = Left other

parseLine :: String -> Either Char [Bracket]
parseLine = traverse parseChar

classifyLine :: [Bracket] -> Line
classifyLine xs = go [] xs
  where
    go [] [] = Complete
    go (x : xs) [] = Incomplete $ x :| xs
    go stack ((Bracket t Open) : rest) = go (t : stack) rest
    go (h : stack) ((Bracket t Close) : rest) =
      if h == t
        then go stack rest
        else Corrupted t
    go [] ((Bracket t Close) : rest) = Corrupted t

scoreOne :: Brace -> Integer -> Integer
scoreOne Round = (+ 1) . (* 5)
scoreOne Square = (+ 2) . (* 5)
scoreOne Squigly = (+ 3) . (* 5)
scoreOne Ancle = (+ 4) . (* 5)

getScore :: Line -> Maybe Integer
getScore (Incomplete brackets) = pure . ($ 0) . appEndo . getDual . foldMap (Dual . Endo . scoreOne) $ brackets
getScore _ = Nothing

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

solve :: [[Bracket]] -> Integer
solve = median . mapMaybe (getScore . classifyLine)

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/day10.txt"
  either print (print . solve) $ traverse parseLine l