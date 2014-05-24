data Tree a = Node (Tree a) a (Tree a)
            | Empty

toStr :: String -> Tree Char -> String
toStr list Empty = 'e' : list
toStr list (Node left a right) = 'n' : a : (toStr (toStr list right) left)

treeToStr :: Tree Char -> String
treeToStr = toStr ""

fromStr :: [Char] -> (Tree Char, [Char])
fromStr ('e' : xs) = (Empty, xs)
fromStr ('n' : xs) = (Node left (head xs) right, right1) 
                    where (right, right1) = fromStr left1
                          (left, left1) = fromStr $ tail xs

strToTree :: [Char] -> Tree Char
strToTree xs = fst (fromStr xs) 

main = do
        let testTree = 
                    (Node
                        (Node
                            Empty
                            'a'
                            Empty
                        )
                        'b'
                            (Node
                                Empty
                                'c'
                                Empty
                            )
                    )
        putStrLn(show $ treeToStr testTree)
        let str = "nbnaeencee"
        putStrLn(show $ treeToStr (strToTree str))