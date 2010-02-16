import Monad
import Control.Monad.State

result002 :: Integer -> Integer
result002 n = sum (filter even (result002' n [2, 1] 1))

result002' n list nextn 
           | nextn < n = result002' n ((head list + nextn):list) (head list)
           | otherwise = list

main :: IO ()
main = print $ result002 4000000
