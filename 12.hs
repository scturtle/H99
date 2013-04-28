data Count a = Single a | Multiple Int a deriving Show

rep :: Count a -> [a]
rep (Single x) = [x]
rep (Multiple n x) = replicate n x

decodeModified :: [Count a] -> [a]
{-decodeModified = concat . map rep-}
decodeModified = concatMap rep
