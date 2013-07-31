import MultiwayTree

bottom_up :: Tree Char -> String
bottom_up (Node x xs) = concatMap bottom_up xs ++ (x:"")
