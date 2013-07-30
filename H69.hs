import Tree

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch x l r) = x:tree2ds l ++ tree2ds r

token :: String -> (String, String)
token ('.':xs) = (".",xs)
token (x:xs) = let
                (t, xs') = token xs
                (t', xs'') = token xs'
                in (x: t ++ t', xs'')

ds2tree :: String -> Tree Char
ds2tree "." = Empty
ds2tree (x:xs) = let
                   (l, xs') = token xs
                   (r, xs'') = token xs'
                   in Branch x (ds2tree l) (ds2tree r)
