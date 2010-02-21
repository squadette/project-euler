import Data.List

fib = fib' 1 1 2

fib' k1 k2 i = (k1 + k2, i + 1) : (fib' k2 (k1 + k2) (i+1))

problem025 n = find (\t -> (length $ show (fst t)) >= n) fib

main :: IO ()
main = do
  print $ problem025 3
  print $ problem025 1000
