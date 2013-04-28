encode all@(x:_) = let (block, rest) = span (==x) all
                    in (length block, head block) : encode rest
encode [] = []
