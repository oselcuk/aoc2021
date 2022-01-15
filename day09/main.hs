import Common
import Data.Char (digitToInt)
import Data.List
import qualified Data.Set as DS
import Debug.Trace

main :: IO ()
main = do
  content <- readFile "input.txt"
  let m = lines content
--   mapM_ (print . findBasin m) $ getLowCoords m
  print $ part1 m
  print $ part2 m
  return ()

type Coord = (Int, Int)

validCoord :: Coord -> Coord -> Bool
validCoord (mx, my) (x, y) = x >= 0 && x < mx && y >= 0 && y < my

getCoord :: [String] -> Coord -> Char
getCoord m (x, y) = m !! x !! y

genNeighborCoords :: Coord -> Coord -> [Coord]
genNeighborCoords mxy (x, y) = filter (validCoord mxy) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

genCoords :: Int -> Int -> [Coord]
genCoords mx my = [(x, y) | x <- [0 .. mx - 1], y <- [0 .. my - 1]]

getLowCoords :: [String] -> [Coord]
getLowCoords m =
  let mx = length m
      my = length (head m)
      get = getCoord m
      low p = all ((> get p) . get) (genNeighborCoords (mx, my) p)
   in filter low $ genCoords mx my

part1 :: [String] -> Int
part1 m = sum $ map ((+ 1) . digitToInt . getCoord m) $ getLowCoords m

findBasin :: [String] -> Coord -> DS.Set Coord
findBasin m p = findBasin' m (DS.singleton p) p
  where
    mx = length m
    my = length (head m)
    get = getCoord m
    findBasin' m seen p =
      let n =
            filter ((/= '9') . get) $
              filter ((> get p) . get) $
                filter (not . (`elem` seen)) $
                  genNeighborCoords (mx, my) p
          seen' = DS.union seen $ DS.fromList n
       in foldl (findBasin' m) seen' n

part2 :: [String] -> Int
part2 m = product $ take 3 $ reverse $ sort $ map (DS.size . findBasin m) $ getLowCoords m