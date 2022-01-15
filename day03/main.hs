import Data.List (elemIndices, sort, transpose)

main :: IO ()
main = do
  content <- readFile "input.txt"
  print $ part1 $ lines content
  print $ part2 $ lines content

readBin :: String -> Int
readBin s = sum $ map (2 ^) $ elemIndices '1' $ reverse s

mostCommon :: [String] -> Int -> Char
mostCommon ss i = sort (map (!! i) ss) !! (length ss `div` 2)

neg :: Char -> Char
neg c = if c == '0' then '1' else '0'

part1 :: [String] -> Int
part1 rows = readBin mids * readBin (map neg mids)
  where
    mids = map (mostCommon rows) [0 .. length (head rows) - 1]

part2 :: [String] -> Int
part2 rows = readBin (o2r 0 id rows) * readBin (o2r 0 neg rows)

o2r :: Int -> (Char -> Char) -> [String] -> String
o2r _ _ [r] = r
o2r i c rs = o2r (i+1) c $ filter ((== (c $ mostCommon rs i)) . (!! i)) rs