import Common
import Data.Char (digitToInt)
import Data.List (group, sort)
import qualified Data.Map.Lazy as DM
import Numeric (readHex, readInt)
import Text.Printf
import Debug.Trace

unpackRead :: [(a, String)] -> a
unpackRead = fst . head

type Version = Int

type Type = String

type Op = [Int] -> Int

data Packet = Literal Version Int | Operator Version Op [Packet]

readBin :: String -> Int
readBin = unpackRead . readInt 2 (`elem` "01") digitToInt

readOperatorPayload :: String -> ([Packet], String)
-- readOperatorPayload s | trace ("readOperatorPayload " ++ s) False = undefined
readOperatorPayload s =
  let (lengthMode, s') = splitAt 1 s
      lengthBits = if lengthMode == "0" then 15 else 11
      (l, payload) = splitAt lengthBits s'
      payloadLen = readBin l
      readPackets0 "" = []
      readPackets0 s = let (p, s') = readPacket s in p : readPackets0 s'
      readPackets1 0 s = ([], s)
      readPackets1 n s = 
        let (p, s') = readPacket s 
            (ps, s'') = readPackets1 (n - 1) s'
         in (p:ps, s'')
  in case lengthMode of
      "0" ->
        let (payloadBits, rest) = splitAt payloadLen payload
         in (readPackets0 payloadBits, rest)
      _ -> readPackets1 payloadLen payload

readLiteralPayload :: Int -> String -> (Int, String)
-- readLiteralPayload _ s | trace ("readLiteralPayload " ++ s) False = undefined
readLiteralPayload p s =
  case splitAt 5 s of
    ('1' : xs, s') -> readLiteralPayload (16 * p + readBin xs) s'
    ('0' : xs, s') -> (16 * p + readBin xs, s')
    (l, r) -> (p, l ++ r)

toOp :: Int -> Op
toOp t = case t of
  0 -> sum
  1 -> product
  2 -> minimum
  3 -> maximum
  5 -> \[x,y] -> fromEnum $ x > y
  6 -> \[x,y] -> fromEnum $ y > x
  7 -> \[x,y] -> fromEnum $ x == y
  _ -> head

readPacket :: String -> (Packet, String)
-- readPacket s | trace ("readPacket    " ++ s) False = undefined
readPacket s =
  let (header, s') = splitAt 6 s
      (v, t) = splitAt 3 header
      (v', t') = (readBin v, readBin t)
   in case t' of
        4 -> let (p, s'') = readLiteralPayload 0 s' in (Literal v' p, s'')
        _ -> let (p, s'') = readOperatorPayload s' in (Operator v' (toOp t') p, s'')

sumVers :: Packet -> Int
sumVers (Literal v _) = v
sumVers (Operator v _ ps) = v + sum (map sumVers ps)

evaluate :: Packet -> Int
evaluate (Literal _ v) = v
evaluate (Operator _ op ps) = op (map evaluate ps)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let bin = concatMap ((printf "%04b" :: Int -> String) . digitToInt) content
      (p, s) = readPacket bin
  -- print bin
  print $ sumVers p
  print $ evaluate p
  return ()