import System.Random

diff_helper _ 0 gen = []
diff_helper [] _ gen = []
diff_helper xs num gen = let (n, gen') = randomR (1, (length xs)) gen
                             x = xs !! (n-1)
                             xs' = take (n-1) xs ++ drop n xs
                             rest = diff_helper xs' (num-1) gen'
                          in x:rest

diff_select n m = do
    gen <- getStdGen
    return $ diff_helper [1..m] n gen

{-take n . nub . randomRs-}
