import Data.List (find)

sqrt' :: Float -> Integer
sqrt' x = if odd t then t else t + 1
          where t :: Integer
                t = (truncate (sqrt x))

largest_odds :: Integer -> [Integer]
largest_odds t = [t, t-2..3]

is_prime :: Integer -> Bool
is_prime k = null (filter (\x -> k `mod` x == 0) (largest_odds (sqrt' $ fromInteger k)))

result005 n = find is_prime (filter (\x -> n `mod` x == 0) (largest_odds (sqrt' $ fromInteger n)))

main :: IO ()
main = print $ result005 600851475143
