import MultiwayTree

nnodes :: Tree a -> Int
nnodes (Node x xs) = 1 + sum (map nnodes xs)

treeToString :: Tree Char -> String
treeToString (Node x xs) = x: concatMap treeToString xs ++ "^"

tokens :: String -> [String]
tokens "^" = []
tokens s = let (a,b) = token 0 "" s in a:tokens b
        where token n a b
                  | not (null a) && n <= 0 = (a, b)
                  | head b == '^' = token (n-1) (a ++ head b:"") (tail b)
                  | otherwise = token (n+1) (a ++ head b:"") (tail b)

stringToTree :: String -> Tree Char
stringToTree (x:xs) = Node x (map stringToTree $ tokens xs)
