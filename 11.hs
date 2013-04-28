import Data.List (group)

data Count a = Single a | Multiple Int a deriving Show

toCount :: [a] -> Count a
toCount xs
    | len==1 = Single (head xs)
    | otherwise = Multiple len (head xs)
    where len = length xs

encodeModified :: Eq a => [a] -> [Count a]
encodeModified = map toCount . group
