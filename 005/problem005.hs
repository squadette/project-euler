result005 n = foldl1 lcm [1..n]

main :: IO ()
main = do print $ result005 10
          print $ result005 20
