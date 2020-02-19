import Data.List (intersperse)   

combine:: Int -> [Bool] -> [Bool] -> [Bool]
combine n (x:xs) (y:ys) =


combine:: [Bool] -> [Bool] -> [Bool]
combine x y = x && y

help_reduce :: [[Bool]] -> [Bool] -> [Bool]
help_reduce (x:xs) curr
  |curr = combine(curr x) ++ help_reduce (xs curr)
