module Day1.Part2 where

countIncreasing :: [Int] -> Int 
countIncreasing l@(_:_:_:t3) = length $ filter id $ zipWith (<) l t3

main :: IO ()
main = do
  file <- readFile "inputs/day1-part1.txt"
  let numbers = read <$> lines file
  print $ countIncreasing numbers
