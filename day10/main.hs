import qualified Data.Map as DM
import Data.Maybe (fromJust)
import Data.List (sort)

import Debug.Trace

close :: DM.Map Char Char
close =
  DM.fromList
    [ ('(', ')'),
      ('{', '}'),
      ('[', ']'),
      ('<', '>')
    ]

scoreBad :: Char -> Int
scoreBad c = case c of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137

scoreIncomplete :: String -> Int
scoreIncomplete [] = 0
scoreIncomplete (x:xs) = scoreIncomplete' x + 5 * scoreIncomplete xs

scoreIncomplete' :: Char -> Int
scoreIncomplete' c = case c of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4

main :: IO ()
main = do
  content <- readFile "input.txt"
  let m = lines content
      res = map (parse []) m
      bad = map fst res
      incomplete = filter (/= 0) $ map snd res
  print $ sum $ map fst res
  print $ sort incomplete !! (length incomplete `div` 2)
  return ()

parse :: [Char] -> String -> (Int, Int)
parse s [] = (0, scoreIncomplete $ reverse s)
parse s (x:xs)
    | DM.member x close = parse ((close DM.! x):s) xs
    | x == head s = parse (tail s) xs
    | otherwise = (scoreBad x, 0)