module Day10.Part1 where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe)

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

getScore :: Line -> Maybe Int
getScore (Corrupted Round) = pure 3
getScore (Corrupted Square) = pure 57
getScore (Corrupted Squigly) = pure 1197
getScore (Corrupted Ancle) = pure 25137
getScore _ = Nothing

solve :: [[Bracket]] -> Int
solve = sum . mapMaybe (getScore . classifyLine)

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/day10.txt"
  either print (print . solve) $ traverse parseLine l