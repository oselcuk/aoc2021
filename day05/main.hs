{-# LANGUAGE TupleSections #-}
import Common ( readCsv, splitOn )

import Debug.Trace
import qualified Data.Map.Strict as DM

main :: IO ()
main = do
  content <- readFile "input.txt"
  let pack = \[x,y] -> (x,y)
      ls = map (pack . map (pack . readCsv) . splitOn " -> ") (lines content) :: [Line]
      (mx,my) = foldl (\(mx,my) ((x1,y1),(x2,y2)) -> (maximum [x1,x2,mx], maximum [y1,y2,my])) (0,0) ls
      points = concatMap genPoints ls
      vents = DM.fromListWith (+) $ map (, 1) points
      ans = length $ filter (>1) $ DM.elems vents
  -- printMap vents mx my
  print ans

type Point = (Int, Int)
type Line = (Point, Point)

type Vents = DM.Map Point Int

genRange :: Int -> Int -> [Int]
genRange l r
    | l > r = [l,l-1..r]
    | otherwise = [l..r]

genPoints :: Line -> [Point]
genPoints ((x1,y1),(x2,y2))
    | x1 == x2 = [(x1,y) | y <- [(min y1 y2)..(max y1 y2)]]
    | y1 == y2 = [(x,y1) | x <- [(min x1 x2)..(max x1 x2)]]
    -- | trace (show ((x1,y1),(x2,y2))) False = undefined
    | otherwise = zip (genRange x1 x2) (genRange y1 y2)

printMap :: Vents -> Int -> Int -> IO [()]
printMap vents mx my =
  mapM print [
    concatMap (\x -> show $ DM.findWithDefault 0 (x,y) vents) [0..mx]
    | y <- [0..my]
  ]