reverseList :: [a] -> [a]
reverseList a = partitionReverse a []
    where
        partitionReverse :: [a] -> [a] -> [a]
        partitionReverse [] a = a
        partitionReverse (x:xs) a = partitionReverse xs (x:a)