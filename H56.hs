module H56 where
import Tree

mirror :: (Eq a) => Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Branch c1 l1 r1) (Branch c2 l2 r2) =
        {-c1==c2 &&-} mirror l1 r2 && mirror r1 l2

symmetric :: (Eq a) => Tree a -> Bool
symmetric Empty = True
symmetric (Branch c l r) = mirror l r
