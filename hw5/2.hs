checker :: (a -> Bool) -> [a] -> Bool
checker cond [] = True
checker cond (x:xs) = if cond x then checker cond xs
                    else False
                 
main = do
    putStrLn (show $ checker (>=0) [11, 12, 13, 15, 0])
    putStrLn (show $ checker (>0) [2, 0, 3, 5, 4, 2])