{-# LANGUAGE NamedFieldPuns #-}

import Common (readCsv, toTriple)
import Data.Foldable (asum)
import Data.List (find, nub, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Set (Set, cartesianProduct, elems, fromList, intersection, union, (\\))
import qualified Data.Set as S

type Coord = (Int, Int, Int)

type Scanner = Set Coord

data Map
  = Union
      { s1 :: Map,
        s2 :: Map,
        u :: Scanner
      }
  | Single Scanner

getCoordRotations :: Coord -> [Coord]
getCoordRotations (x, y, z) =
  [ (x, y, z),
    (x, z, - y),
    (x, - y, z),
    (x, - z, - y),
    (z, y, - x),
    (z, x, y),
    (z, - y, x),
    (z, - x, - y),
    (- x, y, - z),
    (- x, z, y),
    (- x, - y, z),
    (- x, - z, - y),
    (- z, y, x),
    (- z, - x, y),
    (- z, - y, - x),
    (- z, x, - y),
    (y, z, x),
    (y, - x, z),
    (y, - z, - x),
    (y, x, - z),
    (- y, - z, x),
    (- y, - x, - z),
    (- y, z, - x),
    (- y, x, z)
  ]

getRotations :: Scanner -> [Scanner]
getRotations s = s'
  where
    ps = map getCoordRotations (elems s)
    ps' = transpose ps
    s' = map fromList ps'

sub :: Coord -> Coord -> Coord
(lx, ly, lz) `sub` (rx, ry, rz) = (lx - rx, ly - ry, lz - rz)

add :: Coord -> Coord -> Coord
(lx, ly, lz) `add` (rx, ry, rz) = (lx + rx, ly + ry, lz + rz)

findAlign :: Scanner -> Scanner -> Maybe Scanner
findAlign s1 s2 = asum $ map (findOffset s1) (getRotations s2)

findOffset :: Scanner -> Scanner -> Maybe Scanner
findOffset s1 s2 = asum $ S.map (align s1 s2) offsets
  where
    offsets = S.map (uncurry sub) (cartesianProduct s1 s2)

align :: Scanner -> Scanner -> Coord -> Maybe Scanner
align s1 s2 p2 = if overlaps then Just (s1 `union` s2') else Nothing
  where
    s2' = S.map (add p2) s2
    overlap = intersection s1 s2'
    overlaps = length overlap >= 12

-- d1 = S.map (sub p2) (s1 \\ s2')
-- d2 = s2' \\ s1
-- outOfRange (x, y, z) = all ((> 1000) . abs) [x, y, z]
-- overlaps =
--   length overlap >= 2
--     && all outOfRange d2
--     && all outOfRange d1

reduce1 :: [Scanner] -> [Scanner]
reduce1 [] = []
reduce1 [s] = [s]
reduce1 (s1 : xs) =
  let aligns = zip xs $ map (findAlign s1) xs
      (xl, xr) = span (isNothing . snd) aligns
   in if null xr
        then s1 : reduce1 xs
        else (fromJust . snd . head $ xr) : map fst (xl ++ tail xr)

reduce :: [Scanner] -> [Scanner]
reduce = converge . iterate reduce1
  where
    converge (x : xs@(y : _))
      | length x == length y = x
      | otherwise = converge xs
    converge _ = undefined

addToMap :: Map -> Scanner -> Maybe Map
addToMap s@(Single s1) s2 =
  findAlign s1 s2
    >>= ( \u ->
            Just
              Union
                { s1 = s,
                  s2 = Single s2,
                  u
                }
        )

addToMap Union {s1, s2, u} s3 = Nothing
  where m1 = addToMap s1 s3
        m2 = addToMap s2 s3

main :: IO ()
main = do
  content <- readFile "input.txt"
  let chunks = splitOn [""] (lines content)
      scanners = map (fromList . map (toTriple . readCsv) . tail) chunks :: [Scanner]
      reduced = reduce scanners
  print $ length reduced
  print $ length $ head reduced
  return ()