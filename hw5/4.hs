multPairs n = [1..n] >>= (\x -> map (* x) [1..n])

main = do
    putStrLn (show $ multPairs 1)
    putStrLn (show $ multPairs 6)