import Tree

zipPad4 :: Int -> [Int] -> [Int] -> [Int] -> [Int] -> [[Int]]
zipPad4 empty [] [] [] [] = []
zipPad4 empty a b c d = [next a, next b, next c, next d] : zipPad4 empty (rest a) (rest b) (rest c) (rest d)
        where next x = if x==[] then empty else head x
              rest x = if x==[] then [] else tail x

min4 = (((map minimum .) .) .) . zipPad4 1000
max4 = (((map maximum .) .) .) . zipPad4 (-1000)

widen :: Tree a -> (Tree (a, Int), ([Int], [Int]))
widen Empty = (Empty, ([], []))
widen (Branch x Empty Empty) = (Branch (x, 0) Empty Empty, ([], []))
widen (Branch x l r) = let
                (l', (llw, lrw)) = widen l
                (r', (rlw, rrw)) = widen r

                w = head [i | i <- [1..], 
                              all (\(a, b) -> 2*i > a + abs b) (zip lrw rlw)]

                (llw', lrw') = case l' of Empty -> (llw, lrw)
                                          otherwise -> (0:llw, 0:lrw)
                (llw'', lrw'') = ([i-w | i <- llw'], [i-w | i <- lrw'])

                (rlw', rrw') = case r' of Empty -> (rlw, rrw)
                                          otherwise -> (0:rlw, 0:rrw)
                (rlw'', rrw'') = ([i+w | i <- rlw'], [i+w | i <- rrw'])

                lw = min4 llw'' lrw'' rlw'' rrw''
                rw = max4 llw'' lrw'' rlw'' rrw''

                in (Branch (x, w) l' r', (lw, rw))

layout' :: Int -> Int -> Tree (a, Int) -> Tree (a, (Int, Int))
layout' rx d Empty = Empty
layout' rx d (Branch (x, w) l r) = Branch (x, (rx, d)) 
                                          (layout' (rx-w) (d+1) l)
                                          (layout' (rx+w) (d+1) r)

layout :: Tree a -> Tree (a, (Int, Int))
layout tree = layout' rx 1 tree'
        where (tree', (lw, _)) = widen tree
              rx = abs (minimum lw) + 1
