-- Find the last element of a list. 

list :: [Int]
list = [1,2,3,4]

myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast l = last l

myLast' :: [a] -> a
myLast' [] = error "No end for empty lists!"
myLast' [a] = a
myLast' (_:as) = myLast' as

myLast'' :: [a] -> a
myLast'' = head . reverse

myLast''' :: [a] -> a
myLast''' = foldl1' (\ _ x -> x)

-- foldl (((1 + 2) + 3) + 4)
-- foldr t(1 + (2 + (3 + 4)))

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f [a, b] =  f a b
foldl1' f (a:as) = f a (foldl1' f as) 