data NestedList a = Elem a | List [NestedList a] deriving Show

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
{-flatten (List all) = concat $ map flatten all-}
flatten (List all) = concatMap flatten all
