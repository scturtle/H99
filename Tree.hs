module Tree where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf x = Branch x Empty Empty

height :: Tree a -> Int
height Empty = 0
height (Branch a l r) = 1 + (max (height l) (height r))
