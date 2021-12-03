module Day02.Part1 where

import Control.Arrow (Kleisli (..), first, second)
import Control.Monad ((<=<))
import Data.Foldable (foldrM)
import Data.Maybe (maybe)
import Text.Read (readMaybe)

processLine :: String -> Int -> (Int, Int) -> Either String (Int, Int)
processLine "forward" n = pure . first (+ n)
processLine "down" n = pure . second (+ n)
processLine "up" n = pure . second (\x -> x - n)
processLine other _ = const $ Left $ "Unexpected command: " <> other

readError :: Read r => String -> Either String r
readError = maybe (Left "readError") Right . readMaybe

readSecond :: Read r => (a, String) -> Either String (a, r)
readSecond = runKleisli $ second (Kleisli readError)

main :: IO ()
main = do
  l <- fmap (break (== ' ')) . lines <$> readFile "inputs/day2.txt"
  let res = foldrM (uncurry processLine) (0, 0) =<< traverse readSecond l
  either putStrLn (print <> print . uncurry (*)) res
