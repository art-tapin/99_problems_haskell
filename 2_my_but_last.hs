{-
 Find the last-but-one (or second-last) element of a list. Solutions

Example in Haskell:

λ> myButLast [1,2,3,4]
3
λ> myButLast ['a'..'z']
'y'
 -}

list :: [Integer]
list = [1,2,3,5]

myButLast :: [a] -> a
myButLast [] = error "ooops"
myButLast [a,_] = a
myButLast (_:as) = myButLast as

myBustLast' :: [a] -> a
myBustLast' = head . tail . reverse

foldl0 :: (b -> a -> b) -> b -> [a] -> b
foldl0 f acc [] = acc
foldl0 f acc (a:as) = foldl0 f (f acc a) as

foldr0 :: (a -> b -> b) -> b -> [a] -> b
foldr0 f acc [] = acc
foldr0 f acc (x:xs) = x `f` foldr0 f acc xs

