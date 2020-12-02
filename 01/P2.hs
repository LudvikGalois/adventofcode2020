import qualified Data.Set as S
import System.Environment

main :: IO ()
main = getArgs >>= readFile . head >>= print . solve S.empty . map read . lines

solve :: S.Set Int -> [Int] -> Int
solve _ [] = error "No solution"
solve s (x:xs)
  | Just y <- foldr
    (\n m -> if (2020 - n - x) `S.member` s then Just n else m)
    Nothing
    s = x * y * (2020 - x - y)
  | otherwise = solve (S.insert x s) xs
