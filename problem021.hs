

is_amicable x = x == sum (divisors (sum (divisors x))) && x /= (sum (divisors x))

divisors x = 1 : divisors' 2 x

divisors' d x = (if mod x d == 0 then d : div x d : rest_divisors else rest_divisors) 
          where rest_divisors = if d * d > x then [] else divisors' (d + 1) x

amicable_numbers = filter is_amicable [1..10000]

problem021 = sum amicable_numbers


main :: IO ()
main = do
     print $ problem021
