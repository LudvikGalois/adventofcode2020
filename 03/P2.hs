import System.Environment

main :: IO ()
main = getArgs >>= readFile . head >>= print . product . (\x -> map (solve x) [(1,1), (3,1), (5,1), (7,1), (1,2)]) . map cycle . lines

solve :: [String] -> (Int, Int) -> Int
solve [] _ = 0
solve xs@(('#':_):_) (l, d) = 1 + solve (map (drop l) (drop d xs)) (l, d)
solve xs (l, d) = solve (map (drop l) (drop d xs)) (l, d)
