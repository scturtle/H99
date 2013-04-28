isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome all@(x:xs) = x==(last xs) && isPalindrome (init xs)
