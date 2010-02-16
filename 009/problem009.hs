
result009 n = map tuple_prod [(a, b, n - a - b) | a <- [1..n], b <- [1..n-a], a*a + b*b == (n - a - b)*(n - a - b)]
              where tuple_prod (a, b, c) = a * b * c

main :: IO ()
main = print $ result009 1000
