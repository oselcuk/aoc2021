import System.IO

main = do
  content <- readFile "input.txt"
  let depths = map read $ lines content :: [Int]
  print $ part1 depths
  print $ part2 depths

part1 :: [Int] -> Int
part1 depths = length $ filter (uncurry (>)) $ zip depths (head depths : depths)

part2 :: [Int] -> Int
part2 depths = part1 windows
  where
    windows = [x + y + z | (x, y, z) <- zip3 depths (tail depths) (tail $ tail depths)]