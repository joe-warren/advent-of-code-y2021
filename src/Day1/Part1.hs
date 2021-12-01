module Day1.Part1 where

countIncreasing :: [Int] -> Int 
countIncreasing l@(_:t) = length $ filter id $ zipWith (<) l t 

main :: IO ()
main = do
  file <- readFile "inputs/day1-part1.txt"
  let numbers = read <$> lines file
  print $ countIncreasing numbers
