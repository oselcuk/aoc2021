module Common
  ( readCsv,
    splitOn,
    toTuple,
    fromTuple,
    toTriple,
  )
where

import Data.List (groupBy, sortBy, transpose)
import Data.Ord (comparing)
import qualified Data.Text as T

splitOn :: String -> String -> [String]
splitOn on txt = map T.unpack $ T.splitOn (T.pack on) (T.pack txt)

readCsv :: (Read a) => String -> [a]
readCsv s = map read $ splitOn "," s

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

fromTuple :: (a, a) -> [a]
fromTuple (x, y) = [x, y]

toTriple :: [a] -> (a, a, a)
toTriple [x, y, z] = (x, y, z)