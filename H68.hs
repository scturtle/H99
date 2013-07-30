import Tree
import H67

treeToPreorder :: Tree Char -> String
treeToPreorder Empty = ""
treeToPreorder (Branch x l r) = x:treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree Char -> String
treeToInorder Empty = ""
treeToInorder (Branch x l r) = treeToInorder l ++ x:"" ++ treeToInorder r

preInTree :: String -> String -> Tree Char
preInTree "" "" = Empty
preInTree (x:xs) io = let
                        (il,_:ir) = break (==x) io
                        (pl,pr) = splitAt (length il) xs
                        in Branch x (preInTree pl il) (preInTree pr ir)
