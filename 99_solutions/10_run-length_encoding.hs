{-

    Problem 10
    (*) Run-length encoding of a list.

    Use the result of Problem 9 to implement the so-called 
    run-length encoding data compression method. 
    Consecutive duplicates of elements are encoded as lists (N E),
    where N is the number of duplicates of the element E.

    Example:

    * (encode '(a a a a b c c a a d e e e e))
    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

    Example in Haskell:

    λ> encode "aaaabccaadeeee"
    [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}

testList = "aaaabccaadeeee"

encodeMap :: Eq a => [a] -> [(Int, a)]
encodeMap = map enc . pack
    where enc dups = (length dups, head dups)

-- from Solutions, more elegant Map:
encodeMap' :: Eq a => [a] -> [(Int, a)]
encodeMap' = map (\ dup -> (length dup, head dup)) . pack

-- my List Comprehension solution (not so elegant):
encodeLC :: Eq a => [a] -> [(Int, a)]
encodeLC list = [ enc dups | dups <- pack list ]
    where enc dups = (length dups, head dups)

-- from Solutions, a bit more elegant List Compr.:
encodeLC' :: Eq a => [a] -> [(Int, a)]
encodeLC' list = [ (length dups, head dups) | dups <- pack list ]

encodeFold :: Eq a => [a] -> [(Int, a)]
encodeFold = foldr enc [] . pack
            where enc dups acc = (length dups, head dups) : acc

-- also a lesson for Solutions for making foldr more elegant:
encodeFold' :: Eq a => [a] -> [(Int, a)]
encodeFold' = enc . pack
    where enc = foldr (\x acc -> (length x, head x) : acc) []

-- ---------------------------
-- my solution from Problem 9:          
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack list@(a:_) = first : pack rest
    where (first, rest) = span (== a) list