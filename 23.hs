import System.Random

rnd_select :: [a] -> Int -> StdGen -> ([a], StdGen)
rnd_select _ 0 gen = ([], gen)
rnd_select xs num gen = let (n, gen') = randomR (0, (length xs)-1) gen
                            x = xs !! n
                            {-rest = take (n-1) xs ++ drop (n+1) xs-}
                            {-(rest_select, gen'') = rnd_select rest (num-1) gen'-}
                            (rest_select, gen'') = rnd_select xs (num-1) gen'
                    in (x: rest_select, gen)
