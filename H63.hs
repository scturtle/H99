import Tree

completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree n = Branch 'x' (cbt 2) (cbt 3)
        where cbt id
                  | id > n = Empty
                  | otherwise = Branch 'x' (cbt (id*2)) (cbt (id*2+1))
