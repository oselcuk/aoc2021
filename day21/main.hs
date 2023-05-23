import Data.Char (digitToInt)
import Data.List (group, sort)
import Data.List.Split (chunksOf)
import Data.Map (Map, findWithDefault, member, (!))
import Debug.Trace

move :: Int -> Int -> Int
move p r = ((p + r - 1) `mod` 10) + 1

updateScore :: (Int, Int) -> Int -> (Int, Int)
updateScore l r | traceShow (l, r) False = undefined
updateScore (p, s) r = let p' = move p r in (p', s + p')

scores :: (Int, Int) -> [(Int, Int)]
scores p@(p1, p2) = (p1, 0) : (p2, 0) : zipWith updateScore (scores p) rolls
  where
    rolls = map sum $ chunksOf 3 $ cycle [1 .. 1000]

diracRolls :: [(Int, Int)]
diracRolls =
  map (\xs@(x : _) -> (x, length xs)) . group . sort $
    [x + y + z | x <- [1 .. 3], y <- [1 .. 3], z <- [1 .. 3]]

diracRolls' :: [((Int, Int), (Int, Int))]
diracRolls' = [(d1, d2) | d1 <- diracRolls, d2 <- diracRolls]

ta :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
ta op (l1, l2) (r1, r2) = (op l1 r1, op l2 r2)

type PI = (Int, Int)

type PPI = (PI, PI)

type Cache = Map PPI PI

diracScore :: Cache -> PPI -> (Cache, PI)
diracScore c k | k `member` c = (c, c ! k)
diracScore c k = (c, (0, 0))
  where
    diracScore' ((_, s1), (_, s2))
      | s1 >= 21 = (1, 0)
      | s2 >= 21 = (0, 1)
    diracScore' u@((p1, s1), (p2, s2)) =
      let update ((r1, o1), (r2, o2)) = ta (*) (o1, o2) $ diracScore' $ ta updateScore u (r1, r2)
       in foldl1 (ta (+)) $ map update diracRolls'

main :: IO ()
main = do
  content <- readFile "input.txt"
  let [p1, p2] = map (digitToInt . last) (lines content) :: [Int]
      s = takeWhile ((< 1000) . snd) (scores (p1, p2))
      diceRolls = (length s - 1) * 3
      loserScore = snd . last $ s
  --   print (diceRolls * loserScore)
  print $ diracScore' ((p1, 0), (p2, 0))

  return ()