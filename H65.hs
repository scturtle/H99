import Tree

layout' :: Tree a -> (Int, Int) -> (Maybe Int, Maybe Int) -> (Tree (a, (Int, Int)), Int)
layout' Empty _ _ = (Empty, 0)
layout' (Branch x l r) (d, h) (Nothing, Nothing) = case l of
                    Empty -> let
                            (r', rx) = layout' r (d+1, h) (Just 1, Nothing)
                            in (Branch (x,(1, d)) Empty r', 1)
                    otherwise -> let
                                (l', lx) = layout' l (d+1, h) (Nothing, Nothing)
                                tx = lx + 2^(h-d-1)
                                (r', rx) = layout' r (d+1, h) (Just tx, Nothing)
                                in (Branch (x, (tx, d)) l' r', tx)
layout' (Branch x l r) (d, h) (Just fx, Nothing) = let
                                                   tx = fx + 2^(h-d)
                                                   (l', lx) = layout' l (d+1, h) (Nothing, Just tx)
                                                   (r', rx) = layout' r (d+1, h) (Just tx, Nothing)
                                                   in (Branch (x, (tx, d)) l' r', tx)
layout' (Branch x l r) (d, h) (Nothing, Just fx) = let
                                                   tx = fx - 2^(h-d)
                                                   (l', lx) = layout' l (d+1, h) (Nothing, Just tx)
                                                   (r', rx) = layout' r (d+1, h) (Just tx, Nothing)
                                                   in (Branch (x, (tx, d)) l' r', tx)

layout :: Tree a -> Tree (a, (Int, Int))
layout t = fst $ layout' t (1, height t) (Nothing, Nothing)
