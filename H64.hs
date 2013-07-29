import Tree

layout' :: Tree a -> Int -> (Tree (a, (Int, Int)), Int)
layout' Empty d = (Empty, 0)
layout' (Branch x l r) d = let
                    (l', ln) = layout' l (d+1)
                    (r', rn) = layout' r (d+1)
                    in (Branch (x, (ln+1, d)) l' r', ln+rn+1)

layout :: Tree a -> Tree (a, (Int, Int))
layout t = t'
    where (t', n) = layout' t 1
