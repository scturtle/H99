{-bad-}
part :: Eq a => [a] -> ([a], [a])
part all@(x:_) = span (==x) all

pack' :: Eq a => [[a]] -> [a] -> [[a]]
pack' acc [] = acc
pack' acc xs = pack' (acc ++ [block]) rest
	where (block, rest) = part xs

pack xs = pack' [] xs

{-better-}
pack2 all@(x:_) = let (block, rest) = span (==x) all
                  in block : pack rest
pack2 [] = []
