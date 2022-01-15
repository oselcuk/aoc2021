import Data.List (groupBy, transpose, sortBy)
import Data.Ord (comparing)

import Common

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ls = groupBy (\l r -> null l == null r) $ lines content
      nums = readCsv $ head (head ls) :: [Int]
      boards = map (map (map read . words)) $ filter ((>1) . length) ls :: [[[Int]]]
      scores = sortBy (comparing snd) $ map (score nums) boards
  mapM_ print scores
  print $ fst $ last scores
  print $ fst $ head scores

finished :: [[Int]] -> Bool
finished board = any (all (== (-1))) $ board ++ transpose board

mark :: [[Int]] -> Int -> [[Int]]
mark board n = map (map (\x -> if x == n then (-1) else x)) board

score :: [Int] -> [[Int]] -> (Int, Int)
score (x:xs) board
    | finished board' = (x * sum (filter (>0) (concat board')), length xs)
    | otherwise = score xs board'
    where board' = mark board x