module H46 where

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

nand' a b = not $ and' a b
nor' a b = not $ or' a b

xor' :: Bool -> Bool -> Bool
xor' a b = a /= b

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

equ' :: Bool -> Bool -> Bool
equ' a b = a == b

table :: (Bool -> Bool -> Bool) -> IO ()
table f = do
        mapM_ putStrLn $ map (unwords .(map show)) [[a, b, f a b] | a<-[True, False], b<-[True, False]]
