result002 :: Integer -> Integer
result002 n = sum (filter even (result002' [2, 1] 1))
    where result002' list nextn 
            | nextn < n = result002' ((head list + nextn):list) (head list)
            | otherwise = list

main :: IO ()
main = print $ result002 4000000
