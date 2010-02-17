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
  get >>= \cache -> let (ans, cache2) = case Map.lookup n cache of
                                          Just answer -> (answer, cache)
                                          Nothing -> (collatz_len2 n cache)
                    in put cache2 >> return ans

result014 n = maximum (evalState (mapM collatz_len [1..n]) Map.empty)

main :: IO ()
main = print $ result014 1000000
