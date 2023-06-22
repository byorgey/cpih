lengthWithoutE :: [String] -> Int
lengthWithoutE [] = 0
lengthWithoutE (s:ss)
  | 'e' `elem` s = lengthWithoutE ss
  | otherwise = length s + lengthWithoutE ss
