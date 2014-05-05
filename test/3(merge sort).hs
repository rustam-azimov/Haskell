split :: [a] -> ([a],[a])   
split (x:y:zs) = (x:xs,y:ys) where (xs,ys) = split zs
split xs = (xs,[])

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge pr xs [] = xs
merge pr [] ys = ys
merge pr (x:xs) (y:ys)
    | pr x y = x: merge pr xs (y:ys)
    | otherwise = y: merge pr (x:xs) ys

mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort pr []   = []
mergesort pr [x]  = [x]
mergesort pr xs = merge pr (mergesort pr xs1) (mergesort pr xs2)
                where
                    (xs1,xs2) = split xs
 
main = print (mergesort (<=) [21, 15, 16, 34, 21])