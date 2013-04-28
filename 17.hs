{-split = flip splitAt-}

split :: [a] -> Int -> ([a],[a])
split all 0 = ([], all)
split [] n = ([], [])
split (x:xs) n = let (first, second) = split xs (n-1)
                  in (x:first, second)
