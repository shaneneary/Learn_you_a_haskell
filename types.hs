removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

removeUppercase    :: [Char] -> [Char]
removeUppercase    st = [ s | s <- st, s `elem` ['a'..'z']]

test_non_upper :: [Char]
test_non_upper = removeNonUppercase "Hello World"

test_upper :: [Char]
test_upper = removeUppercase "Hello World"

addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  

factorial :: Integer -> Integer
factorial x = product [1..x]

factorial' :: Integer -> Integer 
factorial' x = x * factorial (x - 1)