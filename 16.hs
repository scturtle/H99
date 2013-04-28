dropHelper :: [a] -> Int -> Int -> [a]
dropHelper [] n _ = []
dropHelper (_:xs) n 1 = dropHelper xs n n
dropHelper (x:xs) n c = x : dropHelper xs n (c-1)

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropHelper xs n n
