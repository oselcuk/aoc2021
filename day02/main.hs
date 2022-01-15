import System.IO

main = do
    content <- readFile "input.txt"
    let commands = map ((\[l, r] -> (l, read r :: Int)) . words) $ lines content
    print $ part1 commands
    print $ part2 commands

part1 :: [(String, Int)] -> Int
part1 commands = uncurry (*) $ foldl (\(x, y) (command, n) ->
    case command of "forward" -> (x+n, y)
                    "up"      -> (x, y-n)
                    "down"    -> (x, y+n)
    ) (0, 0) commands


part2 :: [(String, Int)] -> Int
part2 commands = (\(x, y, _) -> x * y) $ foldl (\(x, y, aim) (command, n) ->
    case command of "forward" -> (x+n, y+aim*n, aim)
                    "up"      -> (x, y, aim-n)
                    "down"    -> (x, y, aim+n)
    ) (0, 0, 0) commands