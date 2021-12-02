module Day2.Part2 where

import Control.Arrow (Kleisli (..), first, second)
import Control.Monad ((<=<))
import Data.Foldable (foldlM)
import Data.Maybe (maybe)
import Data.Tuple.Extra (first3, second3, third3)
import Text.Read (readMaybe)

processLine :: String -> Int -> (Int, Int, Int) -> Either String (Int, Int, Int)
processLine "forward" n = \(dep, dist, aim) -> pure (dep + aim * n, dist + n, aim)
processLine "down" n = pure . third3 (+ n)
processLine "up" n = pure . third3 (\x -> x - n)
processLine other _ = const $ Left $ "Unexpected command: " <> other

readError :: Read r => String -> Either String r
readError = maybe (Left "readError") Right . readMaybe

readSecond :: Read r => (a, String) -> Either String (a, r)
readSecond = runKleisli $ second (Kleisli readError)

toPair :: (a, b, c) -> (a, b)
toPair (a, b, _) = (a, b)

main :: IO ()
main = do
  l <- fmap (break (== ' ')) . lines <$> readFile "inputs/day2.txt"
  let res = foldlM (flip $ uncurry processLine) (0, 0, 0) =<< traverse readSecond l
  either putStrLn (print <> print . uncurry (*) . toPair) res
