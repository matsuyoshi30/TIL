main = interact responsePalindrome

responsePalindrome :: String -> String
responsePalindrome = unlines . map (\xs -> if isPal xs then "palindrome" else "not palindrome") . lines

isPal :: String -> Bool
isPal xs = xs == reverse xs
