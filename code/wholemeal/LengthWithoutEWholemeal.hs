lengthWithoutE' :: [String] -> Int
lengthWithoutE' = filter ('e' `notElem`) >>> map length >>> sum
