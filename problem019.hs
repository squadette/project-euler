import Data.Time.Calendar

months_before = [ gregorianMonthLength year month | year <- [1900], month <- [1..12]]
months_after = [ gregorianMonthLength year month | year <- [1901..2000], month <- [1..12]]

day_at_1901 = (remainder months_before 0) !! 11

problem019 = length (filter ( == 6 ) (remainder months_after day_at_1901))

remainder [] init = []
remainder (day:rest) init = new_rem : remainder rest new_rem
          where new_rem = rem (init + day) 7

main :: IO ()
main = do
     print $ problem019
