import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe

step = 1000

collatz_len2 :: Integer -> Map.Map Integer Integer -> (Integer, Map.Map Integer Integer)
collatz_len2 1 cache = (1, cache)
collatz_len2 n cache = (answer, cache3)
                       where (nrest, cache2) = collatz_len2 (if odd n then 3 * n + 1 else n `div` 2) cache
                             answer = 1 + nrest
                             cache3 = Map.insert n answer cache2

collatz_len :: Integer -> State (Map.Map Integer Integer) Integer
collatz_len n = get >>= \cache -> let (ans, cache2) = case Map.lookup n cache of
                                                        Just answer -> (answer, cache)
                                                        Nothing -> (collatz_len2 n cache)
                                  in put cache2 >> return ans

print_step :: Integer -> IO ()
print_step n = do
  putStrLn (show n)
  putStrLn (show (maximum (evalState (mapM collatz_len [n..n+step-1]) Map.empty)))

result014 n = mapM_ print_step [k | k <- [1,step+1..n]]

main :: IO ()
main = do
  result014 1000000
