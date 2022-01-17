{-# LANGUAGE TupleSections #-}

import Common
import Data.List (group, sort)
import qualified Data.Map.Lazy as DM

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ls = lines content
      tpl' = head ls
      ins' = map (splitOn " -> ") (drop 2 ls)
      pairToPairs [s@[l, r], [m]] = (s, DM.fromList [([l, m], 1), ([m, r], 1)])
      ins = DM.fromList $ map pairToPairs ins'
      tpl = DM.fromListWith (+) $ zipWith (\a b -> ([a, b], 1)) tpl' $ tail tpl'
      res = map (calcDiff $ last tpl') $ iterate (expand' ins) tpl

  print $ res !! 40
  return ()

type Pairs = DM.Map String Int

calcDiff :: Char -> Pairs -> Int
calcDiff c pairs =
    let counts = DM.adjust (+1) c $ DM.mapKeysWith (+) head pairs
        counts' = sort $ DM.elems counts
     in maximum counts' - minimum counts'

expand' :: DM.Map String Pairs -> Pairs -> Pairs
expand' ins =
  DM.foldlWithKey
    ( \a k v ->
        DM.unionWith (+) a $ DM.map (* v) (DM.findWithDefault DM.empty k ins)
    )
    DM.empty

expand :: DM.Map String String -> String -> String
expand ins tpl =
  let pairs = zipWith (curry fromTuple) tpl (tail tpl)
      expanded = map (\s -> head s : DM.findWithDefault "" s ins) pairs
   in concat expanded ++ [last tpl]