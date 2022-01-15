module Common 
( readCsv
, splitOn
) where

import qualified Data.Text as T
import Data.List (groupBy, transpose, sortBy)
import Data.Ord (comparing)

splitOn :: String -> String -> [String]
splitOn on txt = map T.unpack $ T.splitOn (T.pack on) (T.pack txt)

readCsv :: (Read a) => String -> [a]
readCsv s = map read $ splitOn "," s