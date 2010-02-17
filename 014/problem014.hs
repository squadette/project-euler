import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe

collatz_len2 :: Integer -> Map.Map Integer Integer -> (Integer, Map.Map Integer Integer)
collatz_len2 1 cache = (1, cache)
collatz_len2 n cache = (answer, cache3)
                       where (nrest, cache2) = collatz_len2 (if odd n then 3 * n + 1 else n `div` 2) cache
                             answer = 1 + nrest
                             cache3 = Map.insert n answer cache2

collatz_len :: Integer -> State (Map.Map Integer Integer) Integer
collatz_len 1 = return 1
collatz_len n = do
  cache <- get
  let (ans, cache2) = collatz_len2 n cache
  put cache2
  return ans

result014 n = runState (collatz_len 13) Map.empty                      -- maximum (map collatz_len [1..n])

--collatz_len' 1 = 1
--collatz_len' n
--    | odd n = 1 + collatz_len' (3 * n + 1)
--    | otherwise = 1 + collatz_len' (n `div` 2)

main :: IO ()
main = print $ result014 1000000
