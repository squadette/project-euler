
import List
import System.IO

data TriangleCell = TriangleCell {
                  number :: Integer,
                  total :: Integer,
                  max_path :: [Integer],
                  max_path_indices :: [Integer]
} deriving (Show)  

data Triangle = Triangle {
     	      		 cells :: [TriangleCell],
                         prev_triangle :: Maybe Triangle
} deriving (Show)

top_triangle :: Integer -> Triangle
top_triangle x = Triangle { cells = [TriangleCell { number = x, total = x, max_path = [x], max_path_indices = [0] }],
                            prev_triangle = Nothing }

process_line' :: [Integer] -> Triangle -> Triangle
process_line' numbers prev_tri = Triangle { cells = sparse_cells, prev_triangle = Just prev_tri }
              where
              sparse_numbers = intersperse (-1000) numbers
              sparse_count = length sparse_numbers
              sparse_cells = [ sparse_cell i | i <- [0..sparse_count - 1]]
              sparse_cell ndx = if (even ndx)
                                  then cell ndx
                                  else TriangleCell { number = 0, total = 0, max_path = [], max_path_indices = [] }
              cell ndx = if ndx == 0
                            then triangle_cell ndx
                            else if ndx == sparse_count - 1
                                    then triangle_cell (ndx - 2)
                                    else error "need choosing"
                         where 
                            n = sparse_numbers !! ndx
                            triangle_cell i = TriangleCell { number = n, 
                                                             total = n + total prev_cell,
                                                             max_path = [n] ++ max_path prev_cell,
                                                             max_path_indices = [toInteger ndx] ++ max_path_indices prev_cell
                                                           }
                                              where prev_cell = (cells prev_tri) !! i
                         
process_line :: String -> Triangle -> Triangle
process_line line triangle = if (length numbers) == 1
             then error "length of numbers == 1"
             else
             process_line' numbers triangle
              where
               numbers = map read (words line)

process_lines :: Handle -> Triangle -> IO Triangle
process_lines inh prev_triangle = do
              ineof <- hIsEOF inh
              if ineof
                 then return prev_triangle
                 else do
                       line <- hGetLine inh
                       process_lines inh (process_line line prev_triangle)
  
main :: IO ()
main = do
       inh <- openFile "triangle-018.txt" ReadMode
       line <- hGetLine inh
       triangle <- process_lines inh (top_triangle (read line))
       hPutStrLn stdout (show triangle)
       hClose inh
