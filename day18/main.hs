import Data.Char (digitToInt)
import Data.Maybe
import Data.List
import Debug.Trace

data Number = Number Int | Pair Number Number deriving (Eq)

instance Show Number where
  show (Number n) = show n
  show (Pair l r) = "[" ++ show l ++ "," ++ show r ++ "]"

magnitude :: Number -> Int
magnitude (Number n) = n
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

parseNumber :: String -> (Number, String)
parseNumber [] = undefined
parseNumber (x : xs) = case x of
  '[' ->
    let (l, xs') = parseNumber xs
        (r, xs'') = parseNumber xs'
     in (Pair l r, xs'')
  ']' -> parseNumber xs
  ',' -> parseNumber xs
  c -> (Number (digitToInt c), xs)

add :: Number -> Number -> Number
add l r = reduce $ Pair l r

explode :: Number -> Number
explode n = n
  where
    find _ n@(Number _) = ([], Nothing)
    find d (Pair (Number l) (Number r)) | d >= 4 = ([], Just (l,r))
    find d (Pair l r) =
      let d' = d + 1
          (fl, nl) = find d' l
          (fr, nr) = find d' r
       in case (isJust nl, isJust nr) of
            (True, _) -> (True : fl, nl)
            (_, True) -> (False : fr, nr)
            _ -> ([], Nothing)
    (pathE, toAdd) = find 0 n
    

-- explode' :: Int -> Number -> (Number, Maybe Int, Maybe Int, Bool)
-- explode' d (Pair (Number l) (Number r)) | d >= 4 = (Number 0, Just l, Just r, True)
-- explode' _ n@(Number _) = (n, Nothing, Nothing, False)
-- explode' d (Pair l r) =
--   let (l', xll, xlr, rx) = explode' (d + 1) l
--       (r', xrl, xrr, lx) = explode' (d + 1) r
--    in case (lx, rx) of
--        (True, _) -> (addL )
--   where
--     addL Nothing n = n
--     addL d (Number n) = Number (n + fromJust d)
--     addL d (Pair l r) = Pair (addL d l) r
--     addR Nothing n = n
--     addR d (Number n) = Number (n + fromJust d)
--     addR d (Pair l r) = Pair l (addR d l)

split :: Number -> Number
split = fst . split'

split' :: Number -> (Number, Bool)
split' s@(Number n)
  | n > 9 =
    let l = n `div` 2
        r = n - l
     in (Pair (Number l) (Number r), True)
  | otherwise = (s, False)
split' (Pair l r) =
  let (l', sl) = split' l
      (r', sr) = split' r
   in if sl then (Pair l' r, sl) else (Pair l r', sr)

reduce :: Number -> Number
reduce n =
  let n' = reduce' n
   in if n /= n' then reduce n' else n
  where
    reduce' n | traceShow n False = undefined
    reduce' n =
      let n' = explode n
       in if n /= n' then n' else split n

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ls = lines content
      parts = map (fst . parseNumber) ls
      parsedWell = all (uncurry (==)) (zip ls (map show parts))
  --   res = foldl (\s@(x:xs) n -> add x n : s) [head parts] (tail parts)
  -- res = add (head parts) (parts !! 1)

  --   print parsedWell
  --   mapM_ print $ reverse res
  -- print res
  return ()