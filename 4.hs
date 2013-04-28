myLength :: (Integral b) => [a] -> b
myLength [] = 0
myLength (_:xs) = (myLength xs) + 1
