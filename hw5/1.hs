expand :: Int -> [[Int]]
expand 0 = [[]]
expand n = expandHelper n n
expandHelper k1 k2 = if k1 > 0  then [1..k2] >>= (\x -> map (x:) (expandHelper (k1 - x) (min x (k1 - x))))
                     else [[]]
                 
main = print (expand 6)