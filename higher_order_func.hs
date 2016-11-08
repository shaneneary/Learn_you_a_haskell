applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

test_apply_twice = applyTwice ("HAHA " ++) "HEY" 

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100  


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x  

-- largestDivisible :: (Integral a) => a  
-- largestDivisible = head (filter p [100000,99999..])  
--     where p x = x `mod` 3829 == 0  

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
	| f x = x : filter' f xs
	| otherwise = filter' f xs