data Tree a = Nil
            | Node (Tree a) a (Tree a)
            deriving (Eq)

size :: Tree a -> Int
size Nil = 0
size (Node left _ right) = size left + size right + 1

height :: Tree a -> Int
height Nil = 0
height (Node left _ right) = max (height left) (height right) + 1

add :: Ord a => a -> Tree a -> Tree a   
add x Nil = Node Nil x Nil
add x (Node leftChild a rightChild) | x < a = add x leftChild
                                    | x > a = add x rightChild
                                    | otherwise = error "Element already exists"

isContain :: Ord a => a -> Tree a -> Bool
isContain _ Nil = False
isContain value (Node left a right) =
                                if (value == a)
                                then True
                                else
                                    if (value < a)
                                    then isContain value left
                                    else isContain value right

biggest (Node _ a right) = if (right == Nil)
                                 then a
                                 else biggest right
                                 
remove :: Ord a => a -> Tree a -> Tree a
remove _ Nil = Nil
remove value (Node left a right) =
                                if (value < a)
                                then Node (remove value left) a right
                                else
                                    if (value > a)
                                    then Node left a (remove value right)
                                    else
                                        if ((left == Nil) && (right == Nil))
                                        then Nil
                                        else
                                            if((left == Nil) && (right /= Nil))
                                            then right
                                            else Node (remove (biggest left) left) (biggest left) right


main = do
        let tree = 
                    (Node
                        (Node
                            Nil
                            3
                            Nil
                        )
                        2
                            (Node
                                Nil
                                4
                                Nil
                            )
                    )      
        putStrLn (show $ size tree) 
        putStrLn (show $ height tree)
        putStrLn (show $ isContain 2 tree)
        putStrLn (show $ isContain 5 tree)
        putStrLn (show $ isContain 6 (add 6 tree))
        putStrLn (show $ isContain 6 (remove 6 tree))