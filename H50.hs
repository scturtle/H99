import Data.List
import Data.Ord (comparing)
import Data.Heap as Heap

huffman :: [(Char, Int)] -> [(Char, String)]
huffman l = sortBy (comparing fst) $ build (Heap.fromList 
                                            [(snd t, [(fst t, "")]) | t <- l] 
                                            :: MinHeap (Int, [(Char, String)]))

build :: MinHeap (Int, [(Char, String)]) -> [(Char, String)]
build heap
        | 1 == size heap = (\(Just x) -> snd x) $ viewHead heap
        | otherwise = let ([lo], heap') = Heap.splitAt 1 heap
                          ([hi], heap'') = Heap.splitAt 1 heap'
                          lo' = (fst lo, [(c, "0" ++ s) | (c, s) <- snd lo])
                          hi' = (fst hi, [(c, "1" ++ s) | (c, s) <- snd hi])
                          heap''' = Heap.insert (fst lo' + fst hi', 
                                            snd lo' ++ snd hi') heap''
                       in build heap'''
