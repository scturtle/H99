combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = [x:xs' | xs' <- combinations (n-1) $ xs] ++ combinations n xs
