removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (x, before ++ after) 
                where (before, x:after) = splitAt (n-1) xs
