gray :: Int -> [String]
gray 1 = ["0", "1"]
gray n = ['0':s | s<- less ] ++ ['1':s | s<- reverse less ]
        where less = gray (n-1)
