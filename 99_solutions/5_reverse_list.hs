{-
    Problem 5
    (*) Reverse a list.

    Example in Haskell:

    λ> myReverse "A man, a plan, a canal, panama!"
    "!amanap ,lanac a ,nalp a ,nam A"
    λ> myReverse [1,2,3,4]
    [4,3,2,1]
-}

testList = "A man, a plan, a canal, panama!"
intList = [1,2,3,4]

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:as) = myReverse as ++ [a]

myReverseFoldl :: [a] -> [a]
myReverseFoldl = foldl revList []
    where revList a b = b:a

-- from anwsers & Prelude:
reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

myReverseFoldr:: [a] -> [a]
myReverseFoldr = foldr swap []
    where swap a b = b ++ [a]
