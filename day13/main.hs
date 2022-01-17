import Common
import qualified Data.Set as DS
import Data.List (transpose)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ls = lines content
      (dots', folds') = span (/= "") ls
      dots = DS.fromList $ map (toTuple . readCsv) dots' :: DS.Set (Int, Int)
      folds = map ((\[x, y] -> (head x, read y) :: (Char, Int)) . splitOn "=" . last . words) $ tail folds'
      res = foldl fold dots folds
  printMap res
  return ()

foldPoint :: (Char, Int) -> (Int, Int) -> (Int, Int)
foldPoint (axis, f) (x, y) = if axis == 'x' then (foldNum x, y) else (x, foldNum y)
  where
    foldNum n
      | n <= f = n
      | otherwise = 2 * f - n

fold :: DS.Set (Int, Int) -> (Char, Int) -> DS.Set (Int, Int)
fold dots f = DS.map (foldPoint f) dots

normalize :: DS.Set (Int, Int) -> (DS.Set (Int, Int), (Int, Int))
normalize s = (DS.fromList $ zip normalizeX normalizeY, (maxX, maxY))
    where
        dots = DS.toList s
        normalize' = map (subtract $ minimum xs)
        (xs, ys) = (map fst dots, map snd dots)
        (normalizeX, normalizeY) = (normalize' xs, normalize' ys)
        (maxX, maxY) = (maximum xs, maximum ys)

printMap :: DS.Set (Int, Int) -> IO ()
printMap s =
    let (dots, (mx, my)) = normalize s
        m = transpose [[if (x,y) `DS.member` dots then '#' else '.' | y <- [0..my]] | x <- [0..mx]]
     in mapM_ putStrLn m