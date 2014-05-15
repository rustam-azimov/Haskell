combine :: Int -> [[Int]]
combine 0 = [[]]
combine 1 = [[1], [2], [3]]
combine n = [[x] ++ y | x <- [1,2,3], y <- combine (n - 1)]

main = print(combine 3)