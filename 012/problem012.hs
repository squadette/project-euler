import Data.List (find, genericLength)

integer_sqrt :: Integer -> Integer
integer_sqrt k = floor (sqrt (fromInteger k))

number_of_divisors :: Integer -> Integer
number_of_divisors p = (2::Integer) * (genericLength $ filter (\k -> p `mod` k == 0) [1..(integer_sqrt p)])

triangle_numbers' :: Integer -> Integer -> [Integer]
triangle_numbers' start delta = (start + delta) : triangle_numbers' (start+delta) (delta + 1)

triangle_numbers = triangle_numbers' 0 1

problem012 n = find (\p -> (number_of_divisors p) > n) triangle_numbers

main :: IO ()
main = print $ problem012 500
