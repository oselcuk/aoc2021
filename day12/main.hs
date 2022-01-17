import Common
import Data.Char (isUpper)
import qualified Data.Map.Strict as DM
import qualified Data.Set as DS

main :: IO ()
main = do
  content <- readFile "input.txt"
  let paths' = concatMap ((\[x, y] -> [(x, [y]), (y, [x])]) . splitOn "-") (lines content)
      paths = DM.fromListWith (++) paths'
  print $ totalPaths paths (DS.singleton "start") "start"
  print $ totalPaths2 paths (DS.singleton "start") False "start"
  return ()

isBig :: String -> Bool
isBig = all isUpper

isSmall :: String -> Bool
isSmall = not . isBig

totalPaths :: DM.Map String [String] -> DS.Set String -> String -> Int
totalPaths paths seen n = sum $ map visit (paths DM.! n)
  where
    visit s
      | s == "end" = 1
      | isBig s = totalPaths paths seen s
      | s `DS.member` seen = 0
      | otherwise = totalPaths paths (DS.insert s seen) s

totalPaths2 :: DM.Map String [String] -> DS.Set String -> Bool -> String -> Int
totalPaths2 paths seen doubled n = sum $ map visit (paths DM.! n)
  where
    visit s
      | s == "end" = 1
      | s == "start" = 0
      | isBig s = totalPaths2 paths seen doubled s
      | s `DS.member` seen = if doubled then 0 else totalPaths2 paths seen True s
      | otherwise = totalPaths2 paths (DS.insert s seen) doubled s