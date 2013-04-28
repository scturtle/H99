rotate :: [a] -> Int -> [a]
rotate all n = drop c all ++ take c all
               where c = if n<0 then n + length all else n
