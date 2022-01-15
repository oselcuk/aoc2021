import Common (readCsv)

import Data.Function (fix)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let fish = readCsv content :: [Int]
  print $ sum $ map (spawns' . (80-)) fish
  print $ sum $ map (spawns' . (256-)) fish
  return ()

spawns :: (Int -> Integer) -> Int -> Integer
spawns f n
  | n <= 7 = 2
  | n <= 9 = 3
  | otherwise = f (n-7) + f (n-9)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0..] !!)

spawns' :: Int -> Integer
spawns' = fix (memoize . spawns)