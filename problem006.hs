
problem006 n = square (sum [1..n]) - sum (map square [1..n])
               where square x = x * x

main :: IO ()
main = print $ problem006 100
