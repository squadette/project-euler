
result016 n = sum digits
              where digits :: [Integer]
                    digits = map read (map (\c -> [c]) (show (2 ^ n)))

main :: IO ()
main = do print $ result016 15
          print $ result016 1000
