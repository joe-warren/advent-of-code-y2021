{-# Language OverloadedStrings#-}
{-# Language TupleSections#-}
module Day04.Part1 where  
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Control.Monad (replicateM, guard)
import Data.Maybe (isNothing)
import Data.List (transpose)
import qualified Data.Text.IO as T
import Control.Applicative (many)
import Data.Foldable (find)


type Parser a = MP.Parsec Void Text a

commaSepNumbers ::  Parser [Int]
commaSepNumbers = MP.decimal `MP.sepBy1` ","

square :: Parser BingoSquare
square = BingoSquare <$> 
     replicateM 5 ("\n" *> ((replicateM 5 (MP.many " " *> (pure <$> MP.decimal))))) <* "\n"

newtype BingoSquare = BingoSquare [[Maybe Int]] deriving Show

isComplete :: BingoSquare -> Bool
isComplete (BingoSquare d) = any (all isNothing) d || any (all isNothing) (transpose d)

fillInSquare :: Int -> BingoSquare -> BingoSquare
fillInSquare n (BingoSquare d) = 
    let filterSquare = find (/= n)
    in BingoSquare $ fmap (fmap filterSquare) d

findWinner :: [Int] -> [BingoSquare] -> Maybe (Int, BingoSquare)
findWinner [] _ = Nothing
findWinner (n:xs) squares = let newSquares = fillInSquare n <$> squares in 
    case find isComplete newSquares of
        Just s -> Just (n, s)
        Nothing -> findWinner xs newSquares

fileContents :: Parser ([Int], [BingoSquare])
fileContents = (,) <$> (commaSepNumbers <* "\n") <*> (many square)
 
scoreSquare :: Int -> BingoSquare -> Int
scoreSquare n (BingoSquare d) = n * (sum . (fmap sum) . (fmap (fmap sum)) $  d )

main :: IO ()
main = do 
    contents <- T.readFile "inputs/day4.txt"
    case MP.parse fileContents "day4" contents of
        Left e -> putStrLn $ MP.errorBundlePretty e
        Right (turns, squares) -> do
            let winner = findWinner turns squares
            print winner
            print $ uncurry scoreSquare <$> winner