module MultiwayTree where

data Tree a = Node a [Tree a] deriving (Eq, Show)
