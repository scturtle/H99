myReverse :: [a] -> [a]
myReverse [] = []
myReverse l = last l: myReverse (init l)
