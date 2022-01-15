import Common
import Data.List
import qualified Data.Map as DM
import Data.Maybe
import Debug.Trace

-- import qualified Data.Set as DS

digits :: DM.Map String Int
digits =
  DM.fromList
    [ ("abcefg", 0),
      ("cf", 1),
      ("acdeg", 2),
      ("acdfg", 3),
      ("bcdf", 4),
      ("abdfg", 5),
      ("abdefg", 6),
      ("acf", 7),
      ("abcdefg", 8),
      ("abcdfg", 9)
    ]

mappings :: [String]
mappings = permutations ['a' .. 'g']

translate :: String -> String -> Maybe Int
translate mapping code = DM.lookup res digits
  where
    translate' c = mapping !! (fromEnum c - fromEnum 'a')
    res = sort $ map translate' code

translateAll :: [String] -> String -> Maybe [Int]
translateAll [] _ = Just []
translateAll (code : codes) mapping =
  case translate mapping code of
    Nothing -> Nothing
    Just x -> case translateAll codes mapping of
      Nothing -> Nothing
      Just xs -> Just (x : xs)

decode :: [String] -> Maybe [Int]
decode samples =
  fromMaybe Nothing (find isJust (map (translateAll samples) mappings))

main :: IO ()
main = do
  content <- readFile "input.txt"
  let displays = map (map (map sort . words) . splitOn " | ") $ lines content
      decodedVals = map (decode . concat) displays
      realVals = map (take 4 . reverse) $ catMaybes decodedVals
      part1 = length $ filter (`elem` [1, 4, 7, 8]) (concat realVals)
      combine' [x1, x2, x3, x4] = x1 + x2 * 10 + x3 * 100 + x4 * 1000
      part2 = sum $ map combine' realVals

  print part1
  print part2