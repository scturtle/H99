import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Ord (comparing)
import qualified Data.Map as Map

{-lsort = sortBy (compare `on` length)-}
lsort = sortBy (comparing length)

{-lfsort :: Ord a => [[a]] -> [[a]]-}
{-[>lfsort xs = sortBy (compare `on` lookupf) xs<]-}
{-lfsort xs = sortBy (comparing lookupf) xs-}
            {-where toFreq = Map.fromListWith (+)-}
                  {-freq = toFreq [(length x, 1) | x <- xs]-}
                  {-lookupf x = Map.lookup (length x) freq-}


lfsort :: Ord a => [[a]] -> [[a]]
lfsort xs = concat . lsort . groupBy ((==) `on` length) . lsort
