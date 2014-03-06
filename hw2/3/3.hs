sumOfDigits :: Integer -> Integer
sumOfDigits 0 = 0
sumOfDigits n = sumOfDigits (n `div` 10) + (n `mod` 10)