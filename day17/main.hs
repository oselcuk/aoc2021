import Data.Char
import Numeric

sums :: [Int]
sums = 1 : zipWith (+) [2..] sums

main :: IO ()
main = do
  content <- readFile "input.txt"
  let isSigned = (`elem` "+-" ++ ['0'..'9'])
      readNum s =
        case span isSigned (dropWhile (not . isSigned) s) of
          ("", _) -> []
          (s, s') -> (read s :: Int) : readNum s'
      [minX, maxX, minY, maxY] = readNum content
      velX = 1 + length (takeWhile (< minX) sums)
      velY = abs minY - 1
      highest = sum [1..velY]
  print highest
  return ()