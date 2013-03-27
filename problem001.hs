result001 :: Int -> Integer
result001 n = sum [x | x <- take n [0..], (x `mod` 3 == 0) || (x `mod` 5 == 0)]

main :: IO ()
main = do print $ result001 10
          print $ result001 1000
