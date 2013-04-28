coprime :: Integral a => a -> a -> Bool
coprime a 0 = a==1
coprime a b = coprime b (a `mod` b)
