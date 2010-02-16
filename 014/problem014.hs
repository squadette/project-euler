
result014 n = maximum (map collatz_len [1..n])

collatz_len 1 = 1
collatz_len n
    | odd n = 1 + collatz_len (3 * n + 1)
    | otherwise = 1 + collatz_len (n `div` 2)

main :: IO ()
main = print $ result014 1000000
