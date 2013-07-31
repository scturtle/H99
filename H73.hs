import MultiwayTree
import Text.Regex

lisp :: Tree Char -> String
lisp (Node x []) = x:""
lisp (Node x xs) = '(':unwords ((x:""):map lisp xs) ++ ")"

findall :: String -> [String]
findall = findall' []
    where p = mkRegex "[ ]?(\\([^\\)]+\\))|[ ]?(.)"
          findall' acc "" = acc
          findall' acc s = let
              Just (_,_,rest,[res1, res2]) = matchRegexAll p s
              res = if null res2 then res1 else res2
              in findall' (res:acc) rest

psil :: String -> Tree Char
psil (x:"") = Node x []
psil ('(':x:xs) = Node x (map psil . findall . init $ xs)
