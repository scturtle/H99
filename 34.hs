totient :: Integral a => a -> Int
totient m = length $ filter (coprime m) [1..(m-1)]
    where coprime a 0 = a==1
          coprime a b = coprime b (a `mod` b)
