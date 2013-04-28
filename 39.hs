import Data.List (dropWhile)

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = all ((/=0) . (n `mod`)) [2..(n-1)]

primesR :: Integral a => a -> a -> [a]
primesR a b = takeWhile (<=b) . dropWhile (<a) $ [p | p <- [2..], isPrime p]
