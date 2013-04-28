{-from solution...-}
{-Replication?-}

combinations :: Int -> [a] -> [([a], [a])]
combinations 0 xs = [([], xs)]
combinations n [] = []
combinations n (x:xs) = ts ++ ds
        where ts = [(x:xs', ys) | (xs', ys) <- combinations (n-1) xs]
              ds = [(xs', x:ys) | (xs', ys) <- combinations n     xs]

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs = [g:gs | (g,rs) <- combinations n xs
                        , gs     <- group ns rs]
