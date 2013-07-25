import Tree
import H56

construct :: [Int] -> Tree Int
construct [] = Empty
construct (x:xs) = Branch x (construct $ filter (<=x) xs) (construct $ filter (>x) xs)
