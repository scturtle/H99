import Tree
import H55
import H56

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric $ cbalTree' n
