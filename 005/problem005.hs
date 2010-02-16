
primes :: Integer -> [Integer]
primes n = takeWhile (< n) (sieve [2..])
    where sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]

all_factors :: Integer -> [[Integer]]
all_factors n = map (max_factors' (primes n)) [2..n]

max_factors :: Integer -> [Integer]
max_factors n = foldl1 max_list (all_factors n)

max_list :: [Integer] -> [Integer] -> [Integer]
max_list = zipWith max

max_factors' :: [Integer] -> Integer -> [Integer]
max_factors' ps num = map (max_factor num) ps

max_factor :: Integer -> Integer -> Integer
max_factor num p 
           | num `mod` p == 0 = 1 + (max_factor (num `div` p) p)
           | otherwise = 0

result005 n = product (zipWith (\x y -> x ^ y) (primes n) (max_factors n))

main :: IO ()
main = do print $ result005 10
          print $ result005 20
