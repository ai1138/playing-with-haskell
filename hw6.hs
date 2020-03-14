firstWords :: FilePath -> IO ()

firstWords s = do 
               a <- readFile s
               let sList = lines a 
               mapM_ (\x ->if words x == [] then putStrLn ("") else putStrLn (head (words x))) sList


