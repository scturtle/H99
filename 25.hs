import Data.List (nub)
import System.Random

rnd_permu xs = do
    gen <- getStdGen
    let len = length xs
        idx = take len . nub $ randomRs (0, len-1) gen
    return $ [xs !! i | i <- idx]
