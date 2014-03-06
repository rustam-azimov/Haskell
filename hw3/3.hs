nextNumber :: Int -> Int
nextNumber 1 = 7
nextNumber 7 = 9
nextNumber 9 = 11
nextNumber n = if m == 9
                   then (nextNumber d) * 10 + 1
                   else d * 10 + nextNumber m
        where
            d = div n 10
            m = mod n 10

myList = 1 : map nextNumber myList