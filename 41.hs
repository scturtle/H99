import Data.List (dropWhile)

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = all ((/=0) . (n `mod`)) [2..(n-1)]

goldbach :: Integral a => a -> (a, a)
goldbach n = head $ filter (\(a, b) -> isPrime a && isPrime b) [(a, n-a) | a<- [2..]]

goldbachList :: Integral a => a -> a -> [(a, a)]
goldbachList a b
    | a>b = []
    | odd a = goldbachList (a+1) b
    | otherwise = goldbach a : goldbachList (a+2) b

goldbachList' :: Integral a => a -> a -> a -> [(a, a)]
goldbachList' a b limit = filter (\(x, y) -> x>limit && y>limit) $ goldbachList a b
