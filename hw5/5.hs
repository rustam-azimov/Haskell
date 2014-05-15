localMax :: [Int] -> Maybe Int
localMax [] = Nothing
localMax [a] = Nothing
localMax [a, b] = Nothing
localMax (x1:x2:x3:xs) = return xs >>= (\end -> 
                                        if (x1 < x2) && (x2 > x3) then 
                                            Just x2 
                                        else localMax (x2:x3:end))

main = do
    putStrLn (show $ localMax [1, 2, 3, 4, 5])
    putStrLn (show $ localMax [1, 2, 4, 3, 5])