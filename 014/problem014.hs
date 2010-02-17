import Data.List
import qualified Data.Map as Map
import Data.Ord
import Control.Monad.State
import Data.Maybe
import System.IO

step = 1000

collatz_len2 :: Integer -> Map.Map Integer Integer -> (Integer, Map.Map Integer Integer)
collatz_len2 1 cache = (1, cache)
collatz_len2 n cache = (answer, cache3)
                       where (nrest, cache2) = collatz_len2 (if odd n then 3 * n + 1 else n `div` 2) cache
                             answer = 1 + nrest
                             cache3 = Map.insert n answer cache2

collatz_len :: Integer -> State (Map.Map Integer Integer) (Integer, Integer)
collatz_len n = get >>= \cache -> case Map.lookup n cache of 
                                    Just answer -> return (n, answer)
                                    Nothing -> let (answer, cache2) = (collatz_len2 n cache)
                                               in put cache2 >> return (n, answer)

print_step :: Map.Map Integer Integer -> Integer -> IO ()
print_step cache 1 = return ()
print_step cache n = do
  let (pairs, cache2) = (runState (mapM collatz_len [n,n-1..n-step+1]) cache)
  let max_pair = maximumBy (comparing snd) pairs
  putStrLn $ (show (snd max_pair)) ++ "\t" ++ (show (fst max_pair)) ++ "\t" ++ (show $ Map.size cache2)
  hFlush stdout
  print_step cache2 (n - step)

result014 n = print_step Map.empty (n + 1)

main :: IO ()
main = do
  result014 1000000
