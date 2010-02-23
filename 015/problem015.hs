
import Control.Monad.State
import qualified Data.Map as Map

type BoundingBox = (Int, Int)
type Cache = Map.Map BoundingBox Integer
type StateProcessor = State Cache Integer

count_paths :: BoundingBox -> StateProcessor
count_paths (w, h)
    | (w == 0 || h == 0) = (return 1)
    | otherwise = do
                    cache <- get
                    case Map.lookup (w, h) cache of
                      Just answer -> return answer
                      Nothing -> do
                        let (c1, cache2) = runState (count_paths (w - 1, h)) cache
                        let (c2, cache3) = runState (count_paths (w, h - 1)) cache2
                        put (Map.insert (w, h) (c1 + c2) cache3)
                        return (c1 + c2)

result015 w h = evalState (count_paths (w, h)) Map.empty

main :: IO ()
main = do
  print $ result015 2 2
  print $ result015 1 2
  print $ result015 5 5
  print $ result015 10 10
  print $ result015 20 20
