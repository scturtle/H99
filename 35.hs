primeFactors :: Integral a => a -> [a]
primeFactors n = helper n 2
    where helper :: Integral a => a -> a -> [a]
          helper 1 _ = []
          helper n i | (mod n i)==0 = i:helper (n `div` i) i
                     | otherwise = helper n (i+1)
