compress :: Eq a => [a] -> [a]
compress (x:ys@(y:_))
    | x == y  = compress ys
    | otherwise = x:compress ys
compress ys = ys

{-Data.List.group-}
