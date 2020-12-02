import qualified Data.Set as S
import System.Environment

main :: IO ()
main = getArgs >>= readFile . head >>= print . solve S.empty . map read . lines

solve :: S.Set Int -> [Int] -> Int
solve _ [] = error "No solution"
solve s (x:xs)
  | (2020 - x) `S.member` s = x * (2020 - x)
  | otherwise = solve (S.insert x s) xs
