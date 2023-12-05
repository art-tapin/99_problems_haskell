{-
    Problem 4
    (*) Find the number of elements in a list.

    Example in Haskell:

    λ> myLength [123, 456, 789]
    3
    λ> myLength "Hello, world!"
    13
-}

list = [1,2,3,4,5]

myLength :: [a] -> Int
myLength lst = snd $ last $ zip lst [1..]

myLengthZip :: [a] -> Int
myLengthZip = fst . last . zip [1..]

myLength' :: [a] -> Int
myLength' = foldr count 0
    where count a acc = succ acc

myLengthFoldr :: [a] -> Int
myLengthFoldr = foldr (const succ) 0

myLengthRec :: [a] -> Int
myLengthRec [] = 0
myLengthRec (_:as) = 1 + myLengthRec as