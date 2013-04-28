import Data.List (dropWhile)

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = all ((/=0) . (n `mod`)) [2..(n-1)]

goldbach :: Integral a => a -> (a, a)
goldbach n = head $ filter (\(a, b) -> isPrime a && isPrime b) [(a, n-a) | a<- [2..]]
