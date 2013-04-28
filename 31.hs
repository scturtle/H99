isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all ((/=0) . (n `mod`)) [2..(n-1)]
