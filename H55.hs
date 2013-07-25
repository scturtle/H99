module H55 where
import Tree

cbalTree :: Int -> IO ()
cbalTree n = mapM_ (putStrLn . show) (cbalTree' n)

cbalTree' :: Int -> [Tree Char]
cbalTree' 0 = [Empty]
cbalTree' n = [Branch 'x' l r | ln <- [0..(n-1)], rn <- [n-1-ln],
                                l <- cbalTree' ln, r <- cbalTree' rn,
                                ln - rn >= -1 && ln - rn <= 1]
