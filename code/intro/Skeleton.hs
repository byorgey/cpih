import Control.Arrow ((>>>))

main = interact $ parse >>> map solve >>> format

parse :: String -> [[Integer]]
parse = _

solve :: [Integer] -> Integer
solve = _

format :: [Integer] -> String
format = _
