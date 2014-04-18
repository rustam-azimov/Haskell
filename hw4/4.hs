unique :: Eq a => [a] -> Bool
unique [] = True
unique (x : xs) = if ((filter (== x) xs == []))
					then unique xs
					else False

main = do
       putStrLn(show $ unique [1, 2, 3, 4, 5, 6, 7, -1])
	   putStrLn(show $ unique [25, 2, 11, 4, 2, 23, 7, 94])