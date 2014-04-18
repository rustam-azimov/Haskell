evenCounter1 :: [Int] -> Int
evenCounter1 = (foldr (+) 0) . (map $ (`mod` 2) . (+ 1))

evenCounter2 :: [Int] -> Int
evenCounter2 = length . (filter $ (==0) . (`mod` 2))

isEven:: Int -> Int
isEven x = if (mod x 2 == 0)
        then 1
        else -1

evenCounter3 :: [Int] -> Int
evenCounter3 x = length (filter (> 0) (map isEven x))


main = do
    putStrLn(show $ evenCounter1 [11, 2, -33, 28, 6, 5, -7, 2, 9, 0, -11])
    putStrLn(show $ evenCounter2 [21, 2, -3, -22,99, 4, -7, -123, 9, 10, -11])
    putStrLn(show $ evenCounter3 [11, 2, -33, 28, 6, 5, 5, 16, 2, 0, -1])