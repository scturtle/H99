module H67 where
import Tree

paired :: String -> Bool
paired = (==0) . sum . map cnt
        where cnt c = case c of '(' -> 1; ')' -> -1; otherwise -> 0

stringToTree :: String -> Tree Char
stringToTree s = case length s of
                     0 -> Empty
                     1 -> leaf (head s)
                     otherwise -> let
                         n = length s
                         inner = init . tail . tail $ s
                         (a', b') = head [(a, tail b) | i <- [0 .. n-1],
                                                        (a, b) <- [splitAt i inner],
                                                        paired a && head b == ',']
                         in Branch (head s) (stringToTree a') (stringToTree b')

treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = x:""
treeToString (Branch x l r) = x:'(':treeToString l ++ "," ++ treeToString r ++ ")"
