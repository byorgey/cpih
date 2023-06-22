import Control.Arrow ((>>>))

main = interact $ parse >>> map solve >>> format

parse :: String -> [[Integer]]
parse = lines >>> map (words >>> map read)

solve :: [Integer] -> Integer
solve [a,b] = abs (a - b)

format :: [Integer] -> String
format = map show >>> unlines
