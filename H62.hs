import Tree

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = (internals l) ++ [x] ++ (internals r)

atlevel :: Tree a -> Int -> [a]
atlevel t n = atlevel' t 1
        where atlevel' Empty lv = []
              atlevel' (Branch x l r) lv 
                = (if n==lv then [x] else []) 
                            ++ atlevel' l (lv+1) ++ atlevel' r (lv+1)
