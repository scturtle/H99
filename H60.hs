import Tree

minNodes :: Int -> Int
minNodes 0 = 0
minNodes 1 = 1
minNodes n = (minNodes (n-1)) + (minNodes (n-2))

maxHeight :: Int -> Int
maxHeight = (map maxHeight' [0..] !!) {-memorization-}
        where maxHeight' 0 = 0
              maxHeight' 1 = 1
              maxHeight' n = foldl max 0
                              $ map (\(hl, hr) -> 1 + (max hl hr))
                                $ filter (\(hl, hr) -> (abs (hl-hr)) <= 1) 
                                  [((maxHeight l), (maxHeight r)) |
                                    l <- [0..n-1], r <- [n-1-l]]


hbalTreeNodes' :: a -> Int -> Int -> [Tree a]
hbalTreeNodes' x 0 0 = [Empty]
hbalTreeNodes' x _ 0 = []
hbalTreeNodes' x 1 1 = [leaf x]
hbalTreeNodes' x _ 1 = []
hbalTreeNodes' x n h = concat [
            [ Branch x l r  | l <- hbalTreeNodes' x ln lh,
                              r <- hbalTreeNodes' x rn rh]
                            | (lh, rh) <- [(h-2, h-1), (h-1, h-2), (h-1, h-1)],
                              ln <- [0..(n-1)], rn <- [n-1-ln],
                              ln >= minNodes lh && lh <= maxHeight ln &&
                              rn >= minNodes rh && rh <= maxHeight rn ]

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = concat [hbalTreeNodes' x n h | h <- [0 .. maxHeight n]]
