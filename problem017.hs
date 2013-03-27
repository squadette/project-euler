

spell_out :: Integer -> String
spell_out 1 = "one"
spell_out 2 = "two"
spell_out 3 = "three"
spell_out 4 = "four"
spell_out 5 = "five"
spell_out 6 = "six"
spell_out 7 = "seven"
spell_out 8 = "eight"
spell_out 9 = "nine"
spell_out 10 = "ten"
spell_out 11 = "eleven"
spell_out 12 = "twelve"
spell_out 13 = "thirteen"
spell_out 14 = "fourteen"
spell_out 15 = "fifteen"
spell_out 16 = "sixteen"
spell_out 17 = "seventeen"
spell_out 18 = "eighteen"
spell_out 19 = "nineteen"
spell_out 20 = "twenty"
spell_out 30 = "thirty"
spell_out 40 = "forty"
spell_out 50 = "fifty"
spell_out 60 = "sixty"
spell_out 70 = "seventy"
spell_out 80 = "eighty"
spell_out 90 = "ninety"
spell_out 1000 = "one thousand"

spell_out n
	  | 20 < n && n < 100 = (spell_out (10 * (n `div` 10))) ++ "-" ++ (spell_out (n `mod` 10))
	  | (n `mod` 100) == 0 = (spell_out (n `div` 100)) ++ " hundred"
	  | otherwise = (spell_out ((n `div` 100) * 100)) ++ " and " ++ (spell_out (n `mod` 100))

problem17 = length (filter (/= ' ') (filter (/= '-') (foldr (++) "" (map spell_out [1..1000]))))

main :: IO ()
main = print problem17
