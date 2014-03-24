data PrintedProduct =  Book { 
    				name :: String,
					author :: String,
					price :: Int 
					}
				|
				Magazine {
					name :: String,
					year :: Int,
					number :: Int,
					price :: Int
					}

printed_product_list_price :: [PrintedProduct] -> Int
printed_product_list_price = foldr ((+) . price) 0