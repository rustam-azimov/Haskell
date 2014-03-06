finder :: Integer -> [Integer] -> Integer
finder _ [] = error "Value not found"
finder n (x:xs) = if n == x
                      then 1
                      else (finder n xs) + 1