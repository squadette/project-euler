

fac n = product [1..n]

result020 n = sum digits
              where digits :: [Integer]
                    digits = map read (map (\c -> [c]) (show (fac 100)))

main :: IO ()
main = print $ result020 100
