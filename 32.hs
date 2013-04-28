gcd' :: Integral a => a -> a -> a
gcd' a 0 = abs a
gcd' a b = gcd' b (a `mod` b)
