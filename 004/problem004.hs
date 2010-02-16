

is_palindrome n = (show n) == (reverse $ show n)

result004 = maximum (filter is_palindrome [ x * y | x <- [101..999], y <- [101..999], x * y `mod` 10 /= 0])

main :: IO ()
main = print $ result004
