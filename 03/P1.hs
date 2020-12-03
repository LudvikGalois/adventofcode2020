import System.Environment

main :: IO ()
main = getArgs >>= readFile . head >>= print . solve . map cycle . lines

solve :: [String] -> Int
solve [] = 0
solve (('#':_):xs) = 1 + solve (map (drop 3) xs)
solve (_:xs) = solve (map (drop 3) xs)
