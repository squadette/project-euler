import Data.List
import qualified Data.Map as Map
import Data.Ord
import Control.Monad.State
import Data.Maybe
import System.IO

step = 1000

collatz_len2 :: Integer -> State (Map.Map Integer Integer) Integer
collatz_len2 1 = return 1
collatz_len2 n = do
  cache <- get
  case Map.lookup n cache of
    Just answer -> return answer
    Nothing -> do
      let (nrest, cache2) = runState (collatz_len2 (if odd n then 3 * n + 1 else n `div` 2)) cache
      let answer = 1 + nrest
      put (Map.insert n answer cache2)
      return answer

collatz_len :: Integer -> State (Map.Map Integer Integer) (Integer, Integer)
collatz_len n = do
  cache <- get
  let (answer, cache2) = runState (collatz_len2 n) cache
  put cache2
  return (n, answer)

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
