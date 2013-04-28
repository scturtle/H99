import Data.List (group)

primeFactors :: Integral a => a -> [a]
primeFactors n = helper n 2
    where helper :: Integral a => a -> a -> [a]
          helper 1 _ = []
          helper n i | (mod n i)==0 = i:helper (n `div` i) i
                     | otherwise = helper n (i+1)

prime_factors_mult :: Integral a => a -> [(a, a)]
prime_factors_mult n = map (\l -> (head l, fromIntegral $ length l)) . group $ primeFactors n

phi :: Integral a => a -> a
phi n = product [(p-1) * p ^ (m-1) | (p,m) <- prime_factors_mult n]
