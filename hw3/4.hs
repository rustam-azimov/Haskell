firstOccur :: [Int] -> Int -> Int
firstOccur (x:xs) value  
    	| x == value = 0
		| otherwise = (firstOccur xs value) + 1

posOfMaxSum :: [Int] -> Int
posOfMaxSum l = firstOccur sumList (foldr max (head sumList) sumList)
				where sumList = zipWith (+) l (0 : l)


main = print (posOfMaxSum [11,5,0,9])