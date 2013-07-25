import Tree

hbalTree :: a -> Int -> [Tree a]
hbalTree x 0 = [Empty]
hbalTree x 1 = [leaf x]
hbalTree x n = [Branch x l r | l <- hbalTree x (n-2), r <- hbalTree x (n-1)] ++
               [Branch x l r | l <- hbalTree x (n-1), r <- hbalTree x (n-2)] ++
               [Branch x l r | l <- hbalTree x (n-1), r <- hbalTree x (n-1)]
