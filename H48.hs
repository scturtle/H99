import H46

enum :: Int -> [[Bool]]
enum 1 = [[True], [False]]
enum n = [True:sol | sol<-sols] ++ [False:sol | sol<-sols]
         where sols = enum (n-1)

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn $ map (unwords . (map show)) [sol ++ [f sol] | sol <- enum n]
