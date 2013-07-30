import MultiwayTree

ipl :: Tree a -> Int
ipl = ipl' 0
    where ipl' n (Node x xs) = n + sum (map (ipl' (n + 1)) xs)
