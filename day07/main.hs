import Common
import Data.List (sort)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let crabs = sort $ readCsv content :: [Int]
  print $ part1 crabs
  print $ part2 crabs
  return ()

part1 :: [Int] -> Int
part1 crabs = sum $ map (abs . (mid -)) crabs
  where
    mid = crabs !! (length crabs `div` 2)

part2 :: [Int] -> Int
part2 crabs = minimum $ map cost [mn .. mx]
  where
    mn = minimum crabs
    mx = maximum crabs
    dist x y = let d = abs (x - y) in (d * (d + 1)) `div` 2
    cost x = sum $ map (dist x) crabs