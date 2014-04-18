add :: [Int] -> Int -> [Int]
add [] x = [x]
add (l : ls) x = if (l < x)
	             then (l : add ls x)
	                else (x : l : ls)

remove :: [Int] -> Int -> [Int]
remove [] x = []
remove (l : ls) x = if (l < x)
	                then (l : remove ls x)
	                    else if (l == x)
		                    then ls
		                      else (l : ls)
        
                    
loop :: [Int] -> IO()
loop list = do
            putStrLn("Enter a command: ")
            str <- getLine 
            case str of
                '0': _ -> putStrLn("exit...")
                '1': _ -> do
                        putStrLn("Enter value to add: ")
                        x <- readLn
                        loop (add list x)
            
                '2': _ -> do 
                        putStrLn("Enter value to remove: ")
                        x <- readLn
                        loop (remove list x)
                '3': _ -> do 
                        putStrLn(show list)
                        loop list
                _  -> do 
                        putStrLn("Bad command.")
                        loop list
main = do
    putStrLn("0 - exit")
    putStrLn("1 - add value to sorted list")
    putStrLn("2 - remove value from list")
    putStrLn("3 - print list")
    loop []