import System.Environment

main :: IO ()
main = getArgs >>= readFile . head >>= print . length . filter valid . lines

parseRule :: String -> (Int, Int)
parseRule s = let (m,(_:n)) = break (== '-') s in (read m, read n)

valid :: String -> Bool
valid s = (== 1) $ length $ filter (`elem` [(m, letter), (n, letter)]) $ zip [1..] pass
  where
    [rule, (letter:_), pass] = words s
    (m, n) = parseRule rule
